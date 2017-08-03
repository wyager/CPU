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
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}

module CPU where

-- CLaSH-provided hardware stuff
import CLaSH.Sized.Unsigned (Unsigned)
import CLaSH.Sized.Vector (Vec((:>), Nil), 
        (!!), replace, repeat, (++))
import CLaSH.Class.Resize (zeroExtend)
import CLaSH.Sized.BitVector (BitVector, (++#), Bit)
import CLaSH.Class.BitPack (pack, unpack)
import CLaSH.Prelude (slice, mealy, moore, bundle, unbundle)
import CLaSH.Promoted.Nat.Literals as Nat
import CLaSH.Signal (Signal, register, sample, sampleN, signal, mux)
import CLaSH.Sized.Index (Index)
import CLaSH.Prelude.BlockRam (blockRam)

-- Plain old Haskell stuff
import Prelude (Show, Eq, print, (+), (-), (*), (==), (/=),
    ($), (.), filter, take, fmap, mapM_, Functor,
    Bool(True,False), not, Maybe(Just,Nothing), (<$>), (<*>), undefined)

-- Used to make sure that something is fully evaluated.
-- Good for making sure that our circuit 
-- doesn't have any undefined behavior.
import Control.DeepSeq (NFData, rnf)

import qualified Test.QuickCheck as QC

import GHC.Generics (Generic)

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



\end{code}


We have two options for how to write our CPU hardware.

We could do it like you would in a standard HDL, where each stage is its own circuit and the stages are connected via explicit wires.

We could also do it more like you would in Haskell, where the entire CPU is defined by a single update function.

The advantage of the first approach is that it's easier to understand where your clock domains lie and the relative stabilization times of different parts of your circuit.

The advantage of the second approach is that it's typically easier to analyze and has less syntactic and cognitive overhead. It's also easier to debug because the entire state of your CPU is explicit.

For this tutorial, I'm going to take the first approach with explicit wires to help us understand how we're physically breaking up the CPU into sub-circuits. In general, I prefer the second approach, but this is up to personal preference.

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

connect :: (Signal b2ram -> Signal ram2c)              -- RAM between B and C
        -> ((Signal a2b, Signal c2b, Signal ram2b)     -- Block B
            -> (Signal b2a, Signal b2c, Signal b2ram))
        -> ((Signal b2c, Signal d2c, Signal ram2c)     -- Block C
            -> (Signal c2b, Signal c2d, Signal c2ram))
        -> ((Signal a2b, Signal d2c, Signal ram2b)     -- Connected block
            -> (Signal b2a, Signal c2d, Signal c2ram))
connect ram blockB blockC inputs = (b2a, c2d, c2ram)
    where
    (a2b, d2c, ram2b) = inputs
    (b2a, b2c, b2ram) = blockB (a2b, c2b, ram2b)
    (c2b, c2d, c2ram) = blockC (b2c, d2c, ram2c)
    ram2c = ram b2ram





data CodeRAMRequest = CodeRAMStall | CodeRAMRead (Ptr CodeRAM)

fetcher :: (Signal Unused, Signal DtoF, Signal Unused) 
        -> (Signal Unused, Signal Validity, Signal CodeRAMRequest)
fetcher = cpuBlock fetcherUpdate fetcherFilter (FetchState (Ptr 0) Invalid) 
fetcherUpdate :: FetchState -> Unused -> DtoF -> Unused -> (FetchState, Unused, CodeRAMRequest)
fetcherUpdate state@(FetchState ptr validity) Unused hazard Unused = (state', Unused, request)
    where
    state' = case hazard of
        D_F_Stall     -> state
        D_F_Jump ptr' -> FetchState ptr' Invalid
        D_F_None      -> FetchState (increment ptr) Valid
    request = case hazard of
        D_F_Stall -> CodeRAMStall
        _         -> CodeRAMRead ptr
fetcherFilter :: FetchState -> Validity
fetcherFilter = instructionRAMOutputValid

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
decoder = cpuBlock decoderUpdate decoderFilter (DecodeState (repeat 0) Nothing)

decoderUpdate :: DecodeState -> Validity -> EtoD -> Word -> (DecodeState, DtoF, Unused)
decoderUpdate state validity eToD fromRAM = (state', dToF, Unused)
    where
    EtoD hazard completedWrite = eToD
    regs' = case completedWrite of
        Nothing -> regs state
        Just (CompletedWrite reg val) -> writeRegister (regs state) reg val
    decodedInstruction' = case hazard of
        E_D_Stall  -> Nothing
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

decoderFilter :: DecodeState -> Maybe (Instruction (Unsigned 64))
decoderFilter = decodedInstruction

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
executer = cpuBlock executerUpdate executerFilter E_Nop

executerUpdate :: ExecuteState -> Maybe (Instruction (Unsigned 64)) -> WtoE -> Unused -> (ExecuteState, EtoD, DataRAMRequest)
executerUpdate _ decodedInstr (WtoE write) Unused = (state', eToD, request)
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
executerFilter :: ExecuteState -> ExecuteState
executerFilter s = s

\end{code}

\begin{code}

data WriteState
    = W_Nop
    | W_Out (Unsigned 64)
    | W_Halt deriving (Generic, Show, Eq)

instance NFData WriteState


writer :: (Signal ExecuteState, Signal Unused, Signal Word)
       -> (Signal WtoE, Signal WriteState, Signal Unused)
writer = cpuBlock writerUpdate writerFilter W_Nop
writerUpdate :: WriteState -> ExecuteState -> Unused -> Word -> (WriteState, WtoE, Unused)
writerUpdate _ executeState Unused fromRAM = (state', wToE, Unused)
    where
    state' = case executeState of
        E_Out v -> W_Out v
        E_Halt  -> W_Halt
        _       -> W_Nop
    wToE = case executeState of
        E_Store r v -> WtoE (Just (CompletedWrite r v))
        E_ReadRAM r -> let Word v = fromRAM in WtoE (Just (CompletedWrite r v))
        _           -> WtoE Nothing
writerFilter :: WriteState -> WriteState
writerFilter s = s

\end{code}

\begin{code}
stallable :: Signal a -> Signal Bool -> Signal a
stallable signal stall = output
    where
    stalled = register False stall
    delayed = register undefined output
    output = mux stalled delayed signal

codeRAM :: Vec n Word -> Signal CodeRAMRequest -> Signal Word
codeRAM contents input = output
    where
    output = stallable ram (stall <$> input)
    ram = blockRam contents (readAddr <$> input) (signal Nothing)
    readAddr CodeRAMStall = 0
    readAddr (CodeRAMRead (Ptr ptr)) = ptr
    stall CodeRAMStall = True
    stall (CodeRAMRead _) = False

dataRAM :: Vec n Word -> Signal DataRAMRequest -> Signal Word
dataRAM contents input = output
    where
    output = blockRam contents (read <$> input) (write <$> input)
    read (Read (Ptr ptr)) = ptr
    read (Write _ _)      = 0
    write (Read _)          = Nothing
    write (Write (Ptr p) v) = Just (p,v)

noRAM :: Signal Unused -> Signal Unused
noRAM x = x

allConnected :: Vec n Word -> Vec m Word
             -> (Signal Unused, Signal Unused, Signal Unused)
             -> (Signal Unused, Signal WriteState, Signal Unused)
allConnected code initialData = fetcher `f2d` decoder `d2e` executer `e2w` writer
    where
    f2d = connect (codeRAM code)
    d2e = connect noRAM
    e2w = connect (dataRAM initialData)

cpu :: Vec n Word -> Vec m Word -> Signal WriteState
cpu code initialData = output
    where
    (_, output, _) = allConnected code initialData (signal Unused, signal Unused, signal Unused)


\end{code}


\begin{code}
program1 :: Vec 27 (Instruction Register)
program1
    =  LoadIm (Register 1) 0     
    :> LoadIm (Register 2) 0x20
    :> Store (Register 1) (Register 2)
    :> LoadIm (Register 1) 1
    :> LoadIm (Register 2) 0x21
    :> Store (Register 1) (Register 2)
    :> LoadIm (Register 3) 0     
    :> LoadIm (Register 2) 0x20  
    :> Add (Register 2) (Register 3) (Register 2)         -- Get the address of the current first term ((Register 2) + (Register 3))
    :> Load (Register 4) (Register 2)           -- Load the first item into (Register 4)
    :> LoadIm (Register 1) 1 
    :> Add (Register 2) (Register 1) (Register 2)         -- Get the address of the second item ((Register 2) + (Register 3) + 1)
    :> Load (Register 1) (Register 2)           -- Load the second item into (Register 1)
    :> Add (Register 4) (Register 1) (Register 4)         -- Add up the first and second items into (Register 4)
    :> LoadIm (Register 1) 1
    :> Add (Register 2) (Register 1) (Register 2)         -- Get the address of the new item ((Register 2) + (Register 3) + 2)
    :> Store (Register 4) (Register 2)          -- Store the new item
    :> Out (Register 4)               -- Print the new item
    :> LoadIm (Register 1) 19
    :> Sub (Register 1) (Register 3) (Register 1)         -- (Register 1) = 19 - loop count
    :> LoadIm (Register 2) haltAddr   -- (Register 2) = Halt address
    :> JmpZ (Register 1) (Register 2)           -- Halt if (Register 1) == 0 (i.e. if loop count is 19)
    :> LoadIm (Register 1) 1
    :> Add (Register 3) (Register 1) (Register 3)         -- Increment loop counter
    :> LoadIm (Register 2) loopStart
    :> Jmp (Register 2)               -- Go back to loop start
    :> Halt
    :> Nil
    where
    haltAddr = 26
    loopStart = 7

codeRAM1 :: Vec 1024 Word
codeRAM1 = fmap encodeInstruction program1 ++ repeat (Word 0)


defaultDataRAM :: Vec 2048 Word
defaultDataRAM = repeat (Word 0)

\end{code}


\begin{code}

type Logic state fromPrev toPrev fromNext toNext fromRAM toRAM
 = (state -> fromPrev -> fromNext -> fromRAM -> (state, toPrev, toRAM), state -> toNext)

connect' :: Logic sB      a2B b2A c2B b2C  ram2B         b2RAM
         -> Logic sC      b2C c2B d2C c2D  ram2C         c2RAM
         -> Logic (sB,sC) a2B b2A d2C c2D (ram2B,ram2C) (b2RAM,c2RAM)
connect' (u1, f1) (u2, f2) = (u,f)
    where
    u (s1,s2) fromPrev1 fromNext2 (fromRAM1, fromRAM2) = ((s1',s2'),toPrev1,(toRAM1,toRAM2))
        where
        (s1', toPrev1, toRAM1) = u1 s1 fromPrev1 from2to1 fromRAM1
        -- Notice: The following line doesn't rely on anything
        -- from the above line, so there is no loop/recursion.
        -- Information can only flow directly in one direction (from stage 2 to stage 1)
        -- Any information from stage 1 to stage 2 has to pass through a register
        -- (in the form of s1), so there's no recursion. If s2 relied on s1' instead of s1,
        -- there would be recursion.
        (s2', from2to1, toRAM2) = u2 s2 from1to2 fromNext2 fromRAM2
        from1to2 = f1 s1
    f (_,s2)= f2 s2

type TotalState = (FetchState, (DecodeState, (ExecuteState, WriteState)))
type RAM2CPU = (Unused, (Word, (Unused, Word)))
type CPU2RAM = (CodeRAMRequest, (Unused, (DataRAMRequest, Unused)))

allConnected' :: Logic TotalState Unused Unused Unused WriteState RAM2CPU CPU2RAM
allConnected' = connect' (fetcherUpdate,  fetcherFilter)  $
                connect' (decoderUpdate,  decoderFilter)  $
                connect' (executerUpdate, executerFilter) $
                         (writerUpdate,   writerFilter)

block' :: (Signal Unused, Signal Unused, Signal RAM2CPU)
       -> (Signal Unused, Signal WriteState, Signal CPU2RAM)
block' = cpuBlock totalUpdate totalFilter initialState
    where
    (totalUpdate, totalFilter) = allConnected'
    initialState = ((FetchState (Ptr 0) Invalid),((DecodeState (repeat 0) Nothing),(E_Nop, W_Nop)))

cpu' :: Vec n Word -> Vec m Word -> Signal WriteState
cpu' code initialData = output
    where
    (_, output, cpu2ram) = block' (signal Unused, signal Unused, ram2cpu)
    shrink = \(c,(_,(d,_))) -> (c,d)
    expand = \(c,d) -> (Unused,(c,(Unused,d)))
    (cpu2code, cpu2data) = (unbundle . fmap shrink) cpu2ram
    ram2cpu = (fmap expand . bundle) (code2cpu, data2cpu)
    code2cpu = codeRAM code cpu2code
    data2cpu = dataRAM initialData cpu2data

instance QC.Arbitrary Word where
    arbitrary = fmap Word QC.arbitrary
    shrink _ = []

equivalent :: Vec 128 Word -> Vec 128 Word -> Bool
equivalent code memory = sampleN 100 (cpu code memory) == sampleN 100 (cpu' code memory)

\end{code}
