=== [Blog](https://yager.io)

== Building a CPU

=== Part 2

[Link to Part 1](https://yager.io/CPU1.html)

Today, we're going to build a pipelined CPU. This will be more realistic and efficient than our previous model CPU. 

As with last time, this entire webpage is a literate Haskell file. You can grab it [here](https://github.com/wyager/CPU/blob/master/CPU2.lhs).

Again, you can play with this file by [installing CLaSH](http://www.clash-lang.org) and running `clashi CPU.lhs`, and hardware simulation instructions are provided below.



=== About This CPU

This CPU will have a pipelined design, which is much more efficient than the previous serial design. 

It will use block RAM, which is a real form of RAM that is available on most FPGAs.

We will implement a branch predictor to help our CPU jump efficiently.

=== The Code

==== Imports

First we're just going to import a bunch of stuff. 

\begin{code}
-- Allows GHC to automatically write code for mapping over register values
{-# LANGUAGE DeriveFunctor #-}

module CPU where

-- CLaSH-provided hardware stuff
import CLaSH.Sized.Unsigned (Unsigned)
import CLaSH.Sized.Vector (Vec((:>), Nil), 
        (!!), replace, repeat, (++))
import CLaSH.Class.Resize (zeroExtend)
import CLaSH.Sized.BitVector (BitVector, (++#), Bit)
import CLaSH.Class.BitPack (pack, unpack)
import CLaSH.Prelude (slice, mealy, moore, unbundle)
import CLaSH.Promoted.Nat.Literals as Nat
import CLaSH.Signal (Signal, register, sample)
import CLaSH.Sized.Index (Index)

-- Plain old Haskell stuff
import Prelude (Show, print, (+), (-), (*), (==), 
    ($), (.), filter, take, fmap, mapM_, Functor,
    Bool(True,False), not, Maybe(Just,Nothing), (<$>), (<*>))

-- Used to make sure that something is fully evaluated.
-- Good for making sure that our circuit 
-- doesn't have any undefined behavior.
import Control.DeepSeq (NFData, rnf)
\end{code}


==== Some CPU-related Types

Our CPU will have 16 64-bit registers. We'll identify them by a register number. Instead of having a different constructor (like `R1`, `R2`, etc.) for each register ID, we'll use CLaSH's `Index` type, which is a number that is bounded at compile time. For example, an `Index 3` can be 0, 1, or 2, but not 3 or higher.

\begin{code}
data Register = Register (Index 16) deriving Show
\end{code}

Some wrapper types, as described in part 1:

\begin{code}
data RAMType = DataRAM | CodeRAM
data Ptr (ram :: RAMType) = Ptr (Unsigned 64) deriving (Show)
data Word = Word (Unsigned 64) deriving Show
data Output = Output (Unsigned 64) deriving Show
instance NFData Output where
    rnf (Output n) = rnf n
\end{code}


==== Instruction Set

The instruction set is the same as last time. However, we will leave the type of input registers as a variable, so that we can either store the register ID or the actual value inside the instruction, depending on which part of the pipeline we're in.

\begin{code}
data Instruction register
    = LoadIm Register (Unsigned 56)
    | Add register register Register
    | Sub register register Register
    | Mul register register Register
    | Load Register register
    | Store register register
    | Jmp register
    | JmpZ register register
    | Out register
    | Halt
    deriving (Show, Functor)

\end{code}

==== CPU Structure

Our CPU will have separate RAM for code (instruction RAM) and data (data RAM). Each RAM takes a full cycle to complete a read or write.

Our CPU will have 4 stages.

* Fetch (F):
** Keep track of the next program counter and dispatch read requests to instruction RAM.
* Decode (D)
** Read the output of instruction RAM and load register values.
* Execute (E):
** Perform any arithmetic operations. Dispatch RAM read/write requests.
* Writeback (W):
** Read the output of data RAM if necessary. Write result values into registers.

Depending on the design, pipelined CPUs may have as few as 2 or as many as 20 stages. Our 4-stage design is a good compromise that lets us observe some of the pitfalls of pipelining.

We'll define the states of our four stages separately.

\begin{code}

data Validity = Valid | Invalid
data Unused = Unused


data WriteState
    = W_Store Register (Unsigned 64)
    | W_Nop
    | W_Out (Unsigned 64)
    | W_Halt


\end{code}


We have two options for how to write our CPU hardware.

We could do it like you would in a standard HDL, where each stage is its own circuit and the stages are connected via explicit wires.

We could also do it more like you would in Haskell, where the entire CPU is defined by a single update function.

The advantage of the first approach is that it's easier to understand where your clock domains lie and the relative stabilization times of different parts of your circuit.

The advantage of the second approach is that it's typically easier to analyze and has less syntactic and cognitive overhead.

For this tutorial, I'm going to take the first approach with explicit wires to help us understand how we're physically breaking up the CPU into sub-circuits.

\begin{code}
readRegister :: Vec 16 a -> Register -> a
readRegister regs (Register i) = regs !! i

writeRegister :: Vec 16 a -> Register -> a -> Vec 16 a
writeRegister regs (Register i) val = replace i val regs

increment :: Ptr a -> Ptr a
increment (Ptr address) = Ptr (address + 1)


\end{code}




==== Machine Code Format

The machine code format is the same as in Part 1.

\begin{code}
encodeInstruction :: Instruction Register -> Word
encodeInstruction instr = Word $ unpack $ case instr of
    LoadIm r v -> tag 0 ++# encodeReg r ++# pack v          -- Pad with zeros
    Add  a b d -> tag 1 ++# encodeReg a ++# encodeReg b ++# encodeReg d ++# 0
    Sub  a b d -> tag 2 ++# encodeReg a ++# encodeReg b ++# encodeReg d ++# 0
    Mul  a b d -> tag 3 ++# encodeReg a ++# encodeReg b ++# encodeReg d ++# 0
    Load   v p -> tag 4 ++# encodeReg v ++# encodeReg p                 ++# 0
    Store  v p -> tag 5 ++# encodeReg v ++# encodeReg p                 ++# 0
    Jmp      p -> tag 6 ++# encodeReg p                                 ++# 0
    JmpZ   z d -> tag 7 ++# encodeReg z ++# encodeReg d                 ++# 0
    Out      v -> tag 8 ++# encodeReg v                                 ++# 0
    Halt       -> tag 9                                                 ++# 0

-- This is just for clarity, and to specify how many bits a tag should be.
tag :: BitVector 4 -> BitVector 4
tag x = x

-- We could have up to 16 regs (0 through 15),
--  but we're only using 4 for now.
encodeReg :: Register -> BitVector 4
encodeReg (Register i) = pack i

decodeInstruction :: Word -> Instruction Register
decodeInstruction (Word val) = case tag of
    0 -> LoadIm a v
    1 -> Add    a b c
    2 -> Sub    a b c
    3 -> Mul    a b c
    4 -> Load   a b
    5 -> Store  a b
    6 -> Jmp    a
    7 -> JmpZ   a b
    8 -> Out    a
    9 -> Halt
    where
    tag = slice Nat.d63 Nat.d60 val
    a   = decodeReg $ slice Nat.d59 Nat.d56 val
    b   = decodeReg $ slice Nat.d55 Nat.d52 val
    c   = decodeReg $ slice Nat.d51 Nat.d48 val
    v   = unpack $ slice Nat.d55 Nat.d0  val

decodeReg :: BitVector 4 -> Register
decodeReg = Register . unpack

\end{code}

\begin{code}
data DtoF = D_F_Stall | D_F_Jump (Ptr CodeRAM) | D_F_None

data FetchState = FetchState {
        nextPC :: Ptr CodeRAM,
        instructionRAMOutputValid :: Validity
    }

cpuBlock :: (state -> fromPrev -> fromNext -> fromRAM -> (state, toPrev, toRAM)) 
         -> (state -> toNext) 
         -> state 
         -> (Signal fromPrev, Signal fromNext, Signal fromRAM)
         -> (Signal toPrev, Signal toNext, Signal toRAM)
cpuBlock update filter initial (fromPrev, fromNext, fromRAM) = (toPrev, toNext, toRAM)
    where
    state = register initial state'
    (state', toPrev, toRAM) = unbundle (update <$> state <*> fromPrev <*> fromNext <*> fromRAM)
    toNext = filter <$> state

data CodeRAMRequest = CodeRAMStall | CodeRAMRead (Ptr CodeRAM)

fetcher :: (Signal Unused, Signal DtoF, Signal Unused) 
        -> (Signal Unused, Signal Validity, Signal CodeRAMRequest)
fetcher = cpuBlock update filter (FetchState (Ptr 0) Invalid) 
    where
    update :: FetchState -> Unused -> DtoF -> Unused -> (FetchState, Unused, CodeRAMRequest)
    update state@(FetchState ptr validity) Unused hazard Unused = (state', Unused, request)
        where
        state' = case hazard of
            D_F_Stall     -> state
            D_F_Jump ptr' -> FetchState ptr' Invalid
            D_F_None      -> FetchState (increment ptr) Valid
        request = case hazard of
            D_F_Stall -> CodeRAMStall
            _         -> CodeRAMRead ptr
    filter :: FetchState -> Validity
    filter = instructionRAMOutputValid

\end{code}



\begin{code}

data DecodeState = DecodeState {
        regs :: Vec 16 (Unsigned 64),
        decodedInstruction :: Maybe (Instruction (Unsigned 64))
    }

data EtoDHazard = E_D_Jump (Ptr CodeRAM) | E_D_Stall | E_D_None

data CompletedWrite = CompletedWrite Register (Unsigned 64)

data EtoD = EtoD EtoDHazard (Maybe CompletedWrite)


decoder :: (Signal Validity, Signal EtoD, Signal Word)
        -> (Signal DtoF, Signal (Maybe (Instruction (Unsigned 64))), Signal Unused)
decoder = cpuBlock update filter (DecodeState (repeat 0) Nothing)
    where
    update :: DecodeState -> Validity -> EtoD -> Word -> (DecodeState, DtoF, Unused)
    update state validity eToD fromRAM = (state', dToF, Unused)
        where
        EtoD hazard completedWrite = eToD
        regs' = case completedWrite of
            Nothing -> regs state
            Just (CompletedWrite reg val) -> writeRegister (regs state) reg val
        decodedInstruction' = case hazard of
            E_D_Stall  -> decodedInstruction state
            E_D_Jump _ -> Nothing
            E_D_None   -> case validity of
                Invalid -> Nothing
                Valid -> Just
                       $ fmap (readRegister regs') 
                       $ decodeInstruction fromRAM
        state' = DecodeState regs' decodedInstruction'
        dToF = case hazard of
            E_D_Jump ptr -> D_F_Jump ptr
            E_D_Stall    -> D_F_Stall
            E_D_None     -> D_F_None
    filter :: DecodeState -> Maybe (Instruction (Unsigned 64))
    filter = decodedInstruction

\end{code}


\begin{code}

data ExecuteState
    = E_Store Register (Unsigned 64)
    | E_ReadRAM Register
    | E_Nop
    | E_Out (Unsigned 64)
    | E_Halt

data WtoE = WtoE (Maybe CompletedWrite)

data DataRAMRequest = Read (Ptr DataRAM)
                    | Write (Ptr DataRAM) Word

executer :: (Signal (Maybe (Instruction (Unsigned 64))), Signal WtoE, Signal Unused)
        -> (Signal EtoD, Signal ExecuteState, Signal DataRAMRequest)
executer = cpuBlock update filter E_Nop
    where
    update :: ExecuteState -> Maybe (Instruction (Unsigned 64)) -> WtoE -> Unused -> (ExecuteState, EtoD, DataRAMRequest)
    update state decodedInstr (WtoE write) Unused = (state', eToD, request)
        where
        eToD = EtoD eToDHazard write
        (eToDHazard, state') = case decodedInstr of
            Nothing -> (E_D_None, E_Nop)
            Just instr -> case instr of
                LoadIm r v  -> (E_D_Stall, E_Store r (zeroExtend v))
                Add a b r   -> (E_D_Stall, E_Store r (a + b))
                Sub a b r   -> (E_D_Stall, E_Store r (a - b))
                Mul a b r   -> (E_D_Stall, E_Store r (a * b))
                Load r ptr  -> (E_D_Stall, E_ReadRAM r)
                Store r ptr -> (E_D_None, E_Nop)
                Jmp dest    -> (E_D_Jump (Ptr dest), E_Nop)
                JmpZ r dest -> (if r == 0 then E_D_Jump (Ptr dest) else E_D_None, E_Nop)
                Out v       -> (E_D_None, E_Out v)
                Halt        -> (E_D_None, E_Halt)
        request = case decodedInstr of
            Just (Load _ ptr) -> Read (Ptr ptr)
            Just (Store v ptr) -> Write (Ptr ptr) (Word v)
            _ -> Read (Ptr 0) -- Could also have a special constructor for "do nothing" if we wanted
    -- The write stage uses the entire execute state
    filter :: ExecuteState -> ExecuteState
    filter s = s

\end{code}


% \begin{code}
% data DecodeHazard = D_Stall | D_Jump (Ptr DataRAM) | D_None
% data RegWrite = RegWrite Register (Unsigned 64)

% fetcher :: Signal (DecodeHazard, Word) -> Signal (FetchHazard, Maybe (Instruction Word))
% fetcher = mealy update _
%     where
%     update :: DecodeState 
%            -> (DecodeHazard, Validity, Word, Maybe RegWrite) 
%            -> (DecodeState, (FetchHazard, Maybe (Instruction Word)))
%     update state (hazard,fromRAM,writes) = case hazard of
%         D_Stall    -> (state', (F_Stall,)) 
%             where
%             state' = DecodeState regs' (decodedInstruction state)
%         D_Jump ptr -> (state', (F_Jump ptr,)) 
%             where
%             state' = DecodeState regs' Nothing
%         D_None     -> (state', (F_None,)) 
%             where
%             state' = DecodeState regs' $ Just 
%                                        $ fmap (readRegister regs') 
%                                        $ decodeInstruction fromRAM

% \end{code}

% ==== CPU Logic

% Our entire CPU logic can be described by a function that takes the current CPU state/RAM contents and returns a new CPU state/RAM contents. In hardware, this corresponds to copying over the entire RAM every clock cycle. It can be done, but it's wasteful, so we're only going to do it in this first CPU.


% \begin{code}
% data ReadOrWrite = Read | Write

% data CPUOutput = CPUOutput {
%         instrReadRequest :: Ptr (Instruction Register),
%         instrStall :: Bool,
%         dataRAMRequest :: Maybe (Ptr Word, ReadOrWrite)
%     }

% data CPUInput = CPUInput {
%         instrRAMRead :: Word,
%         dataRAMRead :: Word
%     }



% cycle :: (CPUState, CPUInput) 
%       -> (CPUState, CPUOutput)
% cycle ((f,d,e,w,regs), (CPUInput instrRAM dataRAM)) = ((f',d',e',w'), (CPUOutput f_addr stall_f e_readwrite))
%     where
%     f'  | stall_f          = f
%         | Jump ptr <- jump = FetchState ptr Invalid
%         | otherwise        = f {ptr = increment (ptr f)}
% \end{code}

% For the decode stage, we have an interesting choice.

% When loading register values, do we load from the new register values or the old register values?

% If we load from the new register values, we increase the dependency length of our decode circuit
% (because first we have to update the registers, *then* read from them, instead of doing both in parallel).

% If we load from the old register values, we shorten our circuit, which lets us have faster clock rates,
% but requires us to stall for an extra cycle after a write.

% We have a similar decision to make for the output of the writeback stage, which we'll cover in a bit.

% In this design, we'll read from the new register values. Again, this will increase our circuit depth (decreasing max clock rate), but it will mean we have to stall for one fewer cycle during a write.

% \begin{code}
%     d'  | stall_d          = d 
%         | Jump ptr <- jump = DecodeState Nothing
%         | otherwise        = DecodeState $ Just 
%                                 $ fmap (readRegister regs') 
%                                 $ decodeInstruction instrRAM
%     d_stall = case decodedInstruction d of
%         Nothing -> False
%         Just instr -> case instr of
%             LoadIm _ _   -> True
%             Add    _ _ _ -> True
%             Sub    _ _ _ -> True
%             Mul    _ _ _ -> True
%             Load   _ _   -> True
%             _            -> False
%     e'  | stall_e = e
%         | otherwise = case decodeInstruction d of
%             Nothing -> E_Nop
%             Just instr -> case instr of
%                 LoadIm r v  -> E_Store r (zeroExtend v)
%                 Add a b r   -> E_Store r (a + b)
%                 Sub a b r   -> E_Store r (a - b)
%                 Mul a b r   -> E_Store r (a * b)
%                 Load r ptr  -> E_ReadRAM r
%                 Store r ptr -> E_Nop
%                 Jmp dest    -> E_Nop
%                 JmpZ r dest -> E_Nop
%                 Out v       -> E_Out v
%                 Halt        -> E_Halt
%     e_stall = case e of
%         E_Store _ _ -> True
%         E_ReadRAM _ -> True
%         _           -> False
%     w' = case e of
%         E_Store r v -> W_Store r v
%         E_ReadRAM r -> let Word v = dataRAM in W_Store r v
%         E_Nop -> W_Nop
%         E_Out v -> W_Out v
%         E_Halt -> W_Halt
%     w_stall = 
%     regs' = case w of
%         W_Store r v -> writeRegister regs r v
%         _ -> regs

%     stall_f = stall_d || d_stall
%     stall_d = stall_e || e_stall
%     stall_e = 


% \end{code}

% Depending on the CPU's current state, we do different things. If the CPU is currently `LoadingInstruction`, we simply read the instruction from RAM and decode it. In future (more realistic) CPUs, this will happen over multiple cycles. For now, it happens in one cycle. After reading the instruction, we return a new state where the CPU is `ExecutingInstruction`. We also increment the program counter register `pc` by one.

% \begin{code}
%     LoadingInstruction -> (CPUState activity' registers', ram)
%         where
%         loadedWord = readRAM ram (pc registers)
%         activity' = ExecutingInstruction (decodeInstruction loadedWord)
%         registers' = registers {pc = increment (pc registers)}
% \end{code}


% If the CPU is currently `ExecutingInstruction`, we inspect the instruction and perform the appropriate action.

% \begin{code}
%     ExecutingInstruction instr -> case instr of
% \end{code}

% `LoadIm` is very simple; we just expand the 56-bit immediate value to 64 bits and store it in the register.

% \begin{code}
%         LoadIm reg val -> (CPUState LoadingInstruction registers', ram)
%             where registers' = writeRegister registers reg (zeroExtend val)
% \end{code}

% `Add`, `Sub`, and `Mul` are all very similar:

% \begin{code}
%         Add a b d -> (CPUState LoadingInstruction registers', ram)
%             where 
%             result = readRegister registers a + readRegister registers b
%             registers' = writeRegister registers d result
%         Sub a b d -> (CPUState LoadingInstruction registers', ram)
%             where 
%             result = readRegister registers a - readRegister registers b
%             registers' = writeRegister registers d result
%         Mul a b d -> (CPUState LoadingInstruction registers', ram)
%             where 
%             result = readRegister registers a * readRegister registers b
%             registers' = writeRegister registers d result
% \end{code}

% We don't have to actually implement integer addition, subtraction, and multiplication ourselves at the circuit level; these are pre-defined primitives in HDLs that hardware compilers know how to optimize aggressively.


% For `Load` and `Store`, we will switch to the `ReadingMemory`/`WritingMemory` states. This isn't strictly necessary in our example hardware, as we could perform a load or store in the same cycle as `ExecutingInstruction`. However, I want to get us used to the idea of memory operations beign slow and requiring special consideration.


% \begin{code}
%         Load valReg ptrReg -> (CPUState (ReadingMemory ptr valReg) registers, ram)
%             where 
%             ptr = Ptr (readRegister registers ptrReg)
%         Store valReg ptrReg -> (CPUState (WritingMemory ptr val) registers, ram)
%             where 
%             ptr = Ptr (readRegister registers ptrReg)
%             val = Word (readRegister registers valReg)
% \end{code}


% For `Jmp`, we just need to modify the program counter. For `JmpZ`, we'll need to modify our program counter depending on whether or not the specified register is zero. Then, we need to go to the `LoadingInstruction` state.

% \begin{code}
%         Jmp destReg -> (CPUState LoadingInstruction registers', ram)
%             where
%             registers' = registers {pc = Ptr (readRegister registers destReg)}
%         JmpZ zeroReg destReg -> (CPUState LoadingInstruction registers', ram)
%             where
%             registers' = if readRegister registers zeroReg == 0
%                 then registers {pc = Ptr (readRegister registers destReg)}
%                 else registers
% \end{code}

% For `Out`, we simply need to switch to the `Outputting` state.

% \begin{code}
%         Out reg -> (CPUState (Outputting output) registers, ram)
%             where
%             output = Output (readRegister registers reg)
% \end{code}

% For `Halt`, we switch to the `Halted` state.

% \begin{code}
%         Halt -> (CPUState Halted registers, ram)
% \end{code}

% That covers all the instructions! Now we just need to handle a few more possible CPU states.

% For `ReadingMemory` and `WritingMemory`, we simply perform the requested RAM operation, write the new value, and go back to `LoadingInstruction`.

% \begin{code}
%     ReadingMemory ptr reg -> (CPUState LoadingInstruction registers', ram)
%         where 
%         Word ramValue = readRAM ram ptr
%         registers' = writeRegister registers reg ramValue
%     WritingMemory ptr val -> (CPUState LoadingInstruction registers, ram')
%         where
%         ram' = writeRAM ram ptr val
% \end{code}

% For `Outputting`, we simply stay in the `Outputting` state for a single cycle and then go back to `LoadingInstruction`. When we write the I/O hardware, we will actually output the value.


% \begin{code}
%     Outputting _ -> (CPUState LoadingInstruction registers, ram)
% \end{code}

% For `Halted`, we just stay `Halted` forever.

% \begin{code}
%     Halted -> (CPUState Halted registers, ram)
% \end{code}


% Now we'll write a couple functions to get information from the CPU state:

% \begin{code}
% isHalted :: CPUState -> Bool
% isHalted (CPUState Halted _) = True
% isHalted _                   = False

% output :: CPUState -> Maybe Output
% output (CPUState (Outputting output) _) = Just output
% output _                                = Nothing
% \end{code}

% ==== Hardware Structure

% Now we're going to define how the CPU lives in hardware. 

% The output of our CPU is a stream of `Bool`s (for whether the CPU has halted) and `Maybe Output`s (for any outputs the CPU might emit). We get to pick the CPU's initial state and RAM contents.

% \begin{code}
% cpuHardware :: CPUState -> RAM -> Signal (Bool, Maybe Output)
% cpuHardware initialCPUState initialRAM = outputSignal
%     where
% \end{code}

% First, we're going to put the CPU state and RAM contents into a hardware register, which is composed of transistor-level memory cells. CLaSH provides a `register` function which takes the initial register contents and the input signal to the register (which the register reads into itself at the start of every clock cycle), and returns the output signal of the register (which is just a copy of the register's contents).

% (Note: While registers are a kind of hardware-level memory, they are different from RAM. In particular, they are much faster and much more expensive. At the transistor level, they are usually built out of D-type flip-flops.)

% \begin{code}
%     systemState = register (initialCPUState, initialRAM) systemState'
% \end{code}

% Every cycle, we are going to replace the old `systemState` with a new state, which is defined as the `cycle` function applied to the old `systemState`.

% \begin{code}
%     systemState' = fmap cycle systemState
% \end{code}

% That part might seem a bit confusing because it's self-referential. Self-referential Haskell code is how we make self-referential circuits, which is how you build hardware with memory. The output of the register is being run through an update function, and the output of that is loaded into the register at the beginning of every clock cycle. If `s[t]` is the state at cycle `t`, then

% ```
% s[0] = (initialCPUState, initialRAM)
% s[1] = cycle s[0]
% s[2] = cycle s[1]
% ...
% ```

% This is what the actual circuit will end up looking like:

% ```
%  ----[cycle]<---
% |               |
% |               |
% |               |
%  -->[register]--
%         ^
%         |
%       clock
% ```

% Now we have to take the state of the CPU, extract the relevant information from the state, and output that information.

% \begin{code}
%     getOutput :: (CPUState, RAM) -> (Bool, Maybe Output)
%     getOutput (state, _) = (isHalted state, output state)
% \end{code}

% Every cycle, our output signal should contain the output data for the new state.

% \begin{code}
%     outputSignal = fmap getOutput systemState'
% \end{code}


% Let's also define an initial CPU state with all registers (including `pc`) set to zero:

% \begin{code}
% defaultCPUState :: CPUState
% defaultCPUState = CPUState LoadingInstruction (Registers 0 0 0 0 (Ptr 0))
% \end{code}


% That's enough code for us to test our CPU in Haskell. Let's write some programs.

% ==== Programming the CPU

% Let's first write a very simple program that just outputs 7, 8, 9.

% \begin{code}
% simpleProgram :: Vec 7 Instruction
% simpleProgram = 
%     LoadIm R1 7 :>
%     LoadIm R2 8 :>
%     LoadIm R3 9 :>
%     Out R1      :>
%     Out R2      :>
%     Out R3      :>
%     Halt        :>
%     Nil
% \end{code}

% And let's compile it into memory:

% \begin{code}
% simpleProgramMem :: Vec 64 Word
% simpleProgramMem = fmap encodeInstruction simpleProgram ++ repeat (Word 0)
% \end{code}

% We fill unused memory with a bunch of zeros.

% Let's generate CPU hardware with this program pre-loaded:

% \begin{code}
% simpleProgramCPU :: Signal (Bool, Maybe Output)
% simpleProgramCPU = cpuHardware defaultCPUState (RAM simpleProgramMem)
% \end{code}

% Because CLaSH is a wrapper around plain old Haskell, we can test our entire CPU design without ever having to put it in hardware. Let's use the `sample` function to get a list of the first 20 outputs from our CPU, one output per clock cycle.

% \begin{code}
% simpleProgramOutput :: [(Bool, Maybe Output)]
% simpleProgramOutput = take 20 $ sample simpleProgramCPU
% \end{code}

% Before we look at the output, let's take a moment to think about what we should expect.

% The CPU starts out in `LoadingInstruction`, so it shouldn't output anything the first cycle. The second cycle, it executes the first `LoadIm` in the `ExecutingInstruction` state. Then, it goes back to `LoadingInstruction`. So each `LoadIm` takes one cycle to load and one cycle to execute. So we should first expect 5 cycles with no output, and then we're back to a `LoadingInstruction`.

% Then, for each `Out` instruction, the CPU has to go through a `LoadingInstruction`, then an `ExecutingInstruction`, then an `Outputting` state. So for every `Out`, there should be two cycles with no output and one cycle with an output. So we should spend 9 cycles on `Out`s.

% Then, for `Halt`, we have to go through `LoadingInstruction`, `ExecutingInstruction`, and then finally to `Halt`. So this should take a total of 3 instructions.

% The total, then, is 17 instructions for this program to execute. On the 17th instruction, the halt bit should be `True`, and it should stay that way.

% Indeed, if we type `mapM_ print simpleProgramOutput` at our interactive prompt, we get

% ```
% (False,Nothing)
% (False,Nothing)
% (False,Nothing)
% (False,Nothing)
% (False,Nothing)
% (False,Nothing)
% (False,Nothing)
% (False,Just (Output 7))
% (False,Nothing)
% (False,Nothing)
% (False,Just (Output 8))
% (False,Nothing)
% (False,Nothing)
% (False,Just (Output 9))
% (False,Nothing)
% (False,Nothing)
% (True,Nothing)
% (True,Nothing)
% (True,Nothing)
% (True,Nothing)
% ```

% Which is exactly what we wanted!

% Let's try a more complicated program, with a loop.

% \begin{code}
% loopProgram :: Vec 9 Instruction
% loopProgram = 
%     LoadIm R1 1  :> -- Constant 1
%     LoadIm R2 5  :> -- Loop counter
%     LoadIm R3 4  :> -- Jump destination ("Out R2")
%     LoadIm R4 8  :> -- Jump destination ("Halt")
%     Out R2       :> -- Output the current counter value
%     JmpZ R2 R4   :> -- Halt if R2 == 0
%     Sub R2 R1 R2 :> -- Decrement R2
%     Jmp R3       :> -- Go to the start of the loop
%     Halt         :>
%     Nil

% loopProgramMem :: Vec 64 Word
% loopProgramMem = fmap encodeInstruction loopProgram ++ repeat (Word 0)

% loopProgramCPU :: Signal (Bool, Maybe Output)
% loopProgramCPU = cpuHardware defaultCPUState (RAM loopProgramMem)

% -- Let's ignore boring outputs
% isBoring :: (Bool, Maybe Output) -> Bool
% isBoring (False, Nothing) = True
% isBoring _                = False

% loopProgramOutput :: [(Bool, Maybe Output)]
% loopProgramOutput = take 7 $ filter (not . isBoring) $ sample loopProgramCPU
% \end{code}

% And if we print out the output:

% ```
% (False,Just (Output 5))
% (False,Just (Output 4))
% (False,Just (Output 3))
% (False,Just (Output 2))
% (False,Just (Output 1))
% (False,Just (Output 0))
% (True,Nothing)
% ```

% Great! Just what we wanted.

% And let's make a program that puts the first 20 fibonacci terms in RAM. We'll use `R1` as a constant values register and `R2` as a pointer register.

% \begin{code}
% fibProgram :: Vec 27 Instruction
% fibProgram
%     =  LoadIm R1 0          -- Store the first 2 terms (0,1) in RAM
%     :> LoadIm R2 0x20
%     :> Store R1 R2
%     :> LoadIm R1 1
%     :> LoadIm R2 0x21
%     :> Store R1 R2
%     :> LoadIm R3 0          -- Loop counter
%     :> LoadIm R2 0x20       -- Start of loop. Load fibonacci array base address
%     :> Add R2 R3 R2         -- Get the address of the current first term (R2 + R3)
%     :> Load R4 R2           -- Load the first item into R4
%     :> LoadIm R1 1 
%     :> Add R2 R1 R2         -- Get the address of the second item (R2 + R3 + 1)
%     :> Load R1 R2           -- Load the second item into R1
%     :> Add R4 R1 R4         -- Add up the first and second items into R4
%     :> LoadIm R1 1
%     :> Add R2 R1 R2         -- Get the address of the new item (R2 + R3 + 2)
%     :> Store R4 R2          -- Store the new item
%     :> Out R4               -- Print the new item
%     :> LoadIm R1 19
%     :> Sub R1 R3 R1         -- R1 = 19 - loop count
%     :> LoadIm R2 haltAddr   -- R2 = Halt address
%     :> JmpZ R1 R2           -- Halt if R1 == 0 (i.e. if loop count is 19)
%     :> LoadIm R1 1
%     :> Add R3 R1 R3         -- Increment loop counter
%     :> LoadIm R2 loopStart
%     :> Jmp R2               -- Go back to loop start
%     :> Halt
%     :> Nil
%     where
%     haltAddr = 26
%     loopStart = 7

% fibProgramMem :: Vec 64 Word
% fibProgramMem = fmap encodeInstruction fibProgram ++ repeat (Word 0)

% fibProgramCPU :: Signal (Bool, Maybe Output)
% fibProgramCPU = cpuHardware defaultCPUState (RAM fibProgramMem)


% fibProgramOutput :: [(Bool, Maybe Output)]
% fibProgramOutput = take 21 $ filter (not . isBoring) $ sample fibProgramCPU
% \end{code}

% And indeed, we get

% ```
% (False,Just (Output 1))
% (False,Just (Output 2))
% (False,Just (Output 3))
% (False,Just (Output 5))
% (False,Just (Output 8))
% (False,Just (Output 13))
% ...
% (False,Just (Output 6765))
% (False,Just (Output 10946))
% (True,Nothing)
% ```

% Sweet! 

% ==== Interfacing With Hardware

% OK, now for the final step in today's tutorial: putting this thing in actual hardware.

% This part is pretty simple. First, we want to take the `Signal (Bool, Maybe Output)` stream from our CPU, which has an ambiguous/arbitrary bit-level representation, and transform it into a stream of something that has an unambiguous bit-level representation. This will let us plug our CPU code into other hardware code, in the hardware language of our choice.

% \begin{code}
% hardwareTranslate :: (Bool, Maybe Output) -> (Bit, Bit, BitVector 64)
% hardwareTranslate (halted, output) = (haltedBit, outputActive, outputValue)
%     where
%     haltedBit = if halted then 1 else 0
%     (outputActive, outputValue) = case output of
%         Nothing -> (0, 0)
%         Just (Output val) -> (1, pack val)
% \end{code}

% Now we can apply this function to our stream of CPU output values to get a stream of bit values. If we call this stream of bit values `topEntity`, CLaSH knows that we want to compile this to a hardware object.

% \begin{code}
% topEntity :: Signal (Bit, Bit, BitVector 64)
% topEntity = fmap hardwareTranslate fibProgramCPU
% \end{code}

% To compile the CPU, we just run `clash --verilog CPU.lhs`.

% This generates a bunch of `.v` files in `./verilog/CPU`.

% We use the following Verilog file (call it `cpu.v`) to run our CPU hardware:

% ```verilog
% `timescale 1ns/1ns

% module main();

%     // Toggle the reset line
%     initial begin
%         reset_reg = 1;
%         reset_reg = #1 0;
%         reset_reg = #2 1;
%     end
%     reg reset_reg;
%     wire reset = reset_reg;
    
%     // Clock line
%     reg theClock = 0;
%     assign clk = theClock;
%     always begin
%         #50;
%         theClock = !theClock;
%     end

%     wire halt;
%     wire output_valid;
%     wire [63:0] output_data;

%     CPU_topEntity cpu(clk, reset, halt, output_valid, output_data);
    
%     always@(posedge clk) begin
%         if (output_valid == 1) begin
%             $display("0x%h", output_data);
%         end else begin
%             $display(".");
%         end
%     end

%     always@(posedge clk) begin
%         if (halt == 1) $finish;
%     end

% endmodule
% ```

% This prints the output data if there is any or a dot if there isn't any.

% To compile the iverilog simulation, run

% ```bash
% clash --verilog CPU.lhs
% iverilog -o cpu -s main cpu.v verilog/CPU/*.v
% ```

% And to run it, run

% ```bash
% timeout 10 ./cpu
% ```

% After a couple seconds, you should start seeing CPU output!

% === Up Next

% In part 2, we're going to cover more realistic RAM behavior and CPU pipelining, which will bring us closer to modern processors.

