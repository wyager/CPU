Today, we're going to build a simple CPU. We're going to write it in Haskell and compile it to hardware.

This CPU design is an extremely simple serial state machine. It's easily 50x slower than an optimized CPU running at the same clock rate. We'll start with this simple design to get a handle on the problem, and switch to more complicated (but faster) designs in later installments.

We will also pretend that RAM access is instantaneous. We will deal with realistic RAM in later installments.

The fact that we're writing this CPU in Haskell instead of in an HDL like Verilog means that there will be substantial stylistic differences from how CPUs are normally written. However, almost all of these differences make it vastly simpler and faster to write CPUs in Haskell. The downside is that the generated hardware isn't (yet) as space-efficient as hand-rolled circuitry. It's sort of like the difference between writing optimized assembly by hand versus using a high-level language. However, as compiler technology improves, we can close the efficiency gap, save a lot of human labor, and write less buggy hardware.

\begin{code}
module Main where

import CLaSH.Sized.Unsigned (Unsigned)
import CLaSH.Sized.Vector (Vec((:>), Nil), (!!), replace, repeat, (++))
import CLaSH.Class.Resize (zeroExtend)
import CLaSH.Sized.BitVector (BitVector, (++#), Bit)
import CLaSH.Class.BitPack (pack, unpack)
import CLaSH.Prelude (slice)
import CLaSH.Promoted.Nat.Literals as Nat
import CLaSH.Signal (Signal, register, sample)

-- Plain old Haskell stuff
import Prelude (Show, print, (+), (-), (*), (==), ($), (.), filter, fmap, Bool(True,False), not, take, mapM_)

import Control.DeepSeq (NFData, rnf)

import Data.Maybe (Maybe(Just,Nothing))

main = print R1
\end{code}


Our CPU will have 4 64-bit registers. We'll identify them by a register number.

\begin{code}
data Register = R1 | R2 | R3 | R4 deriving Show
\end{code}

We'll define a custom type for 64-bit memory addresses:

\begin{code}
data Ptr = Ptr (Unsigned 64) deriving (Show)
\end{code}

A custom type for 64-bit memory words:

\begin{code}
data Word = Word (Unsigned 64) deriving Show
\end{code}

As well as a custom type for 64-bit I/O output:

\begin{code}
data Output = Output (Unsigned 64) deriving Show
-- Making Output an instance of NFData allows CLaSH 
--  to force full CPU evaluation during testing.
instance NFData Output where
    rnf (Output n) = rnf n
\end{code}

These all have the same underlying 64-bit representation, so the type is just to help us keep track of whether something is a value or a pointer to a value.

Let's define our instruction set. The first instruction is to load a 56-bit immediate value into a register.

\begin{code}
data Instruction 
    = LoadIm Register (Unsigned 56)
\end{code}

The second instruction is to add two registers and put the result into a third register.

\begin{code}
    | Add Register Register Register
\end{code}

And the same for subtraction:

\begin{code}
    | Sub Register Register Register
\end{code}

And integer multiplication:

\begin{code}
    | Mul Register Register Register
\end{code}

Next, we'll add memory load and stores. The first argument is the source or destination register, and the second argument is the register that holds the pointer to memory.

\begin{code}
    | Load Register Register
    | Store Register Register
\end{code}

We'll add an unconditional and conditional branch. For the unconditional branch, the CPU jumps to the address in the register. For the conditional branch, if the first register is equal to zero, the CPU jumps to the address in the second register.

\begin{code}
    | Jmp Register
    | JmpZ Register Register
\end{code}

We'll add an "Output" instruction, which will output a register's contents through the 64-bit IO port.

\begin{code}
    | Out Register
\end{code}

Finally, we'll add a "Halt" instruction, which will halt the CPU.

\begin{code}
    | Halt
\end{code}

Next, we'll define the possible activities our CPU can be engaged in:

\begin{code}
data CPUActivity
    = LoadingInstruction
    | ExecutingInstruction Instruction
    | ReadingMemory Ptr Register
    | WritingMemory Ptr Word
    | Outputting Output
    | Halted
\end{code}

As well as the state of our CPU registers:

\begin{code}
data Registers = Registers {
        r1 :: Unsigned 64,
        r2 :: Unsigned 64,
        r3 :: Unsigned 64,
        r4 :: Unsigned 64,
        pc :: Ptr 
    }
\end{code}

The total state of our CPU is the combination of those two things.

\begin{code}
data CPUState = CPUState CPUActivity Registers
\end{code}

We're also going to have our unrealistic 1kiB internal "RAM" which can finish a read or write request in the same cycle it was dispatched. Technically you could do this in real hardware, but it would be fairly expensive and would slow down your maximum clock rate by a lot. Real internal RAM takes at least one cycle to complete an operation, and external RAM usually takes multiple cycles. We'll deal with that next time.


\begin{code}
data RAM = RAM (Vec 1024 Word)
\end{code}



We'll write a few helper functions that let us perform basic hardware operations, like reading and writing from registers and RAM.

\begin{code}
readRegister :: Registers -> Register -> Unsigned 64
readRegister (Registers r1 r2 r3 r4 _) reg = case reg of
    R1 -> r1
    R2 -> r2
    R3 -> r3
    R4 -> r4

writeRegister :: Registers -> Register -> Unsigned 64 -> Registers
writeRegister regs reg word = case reg of
    R1 -> regs {r1 = word}
    R2 -> regs {r2 = word}
    R3 -> regs {r3 = word}
    R4 -> regs {r4 = word}
\end{code}

\begin{code}
readRAM :: RAM -> Ptr -> Word
readRAM (RAM contents) (Ptr address) = contents !! address

writeRAM :: RAM -> Ptr -> Word -> RAM
writeRAM (RAM contents) (Ptr address) val = RAM (replace address val contents)

increment :: Ptr -> Ptr
increment (Ptr address) = Ptr (address + 1)
\end{code}

Now we have to write code to encode and decode instructions.

\begin{code}
encode :: Instruction -> Word
encode instr = Word $ unpack $ case instr of
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

tag :: BitVector 4 -> BitVector 4
tag x = x

encodeReg :: Register -> BitVector 4
encodeReg R1 = 1
encodeReg R2 = 2
encodeReg R3 = 3
encodeReg R4 = 4

decodeInstruction :: Word -> Instruction
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
decodeReg 1 = R1
decodeReg 2 = R2
decodeReg 3 = R3
decodeReg 4 = R4

\end{code}

Our entire CPU can be described by a function that takes the CPU state and the RAM contents, and returns a new CPU state, RAM contents, maybe an output, and whether the CPU is halted. Again, this is fairly unrealistic; in hardware, this corresponds to copying over the entire RAM every clock cycle. It can be done, but it's wasteful, so we're only going to do it in this first CPU.


\begin{code}
cycle :: (CPUState, RAM) -> (CPUState, RAM)
cycle (CPUState activity registers, ram) = case activity of
\end{code}

Depending on the CPU's current activity, we do different things. If the CPU is currently `LoadingInstruction`, we simply read the instruction from RAM and decode it. In future (more realistic) CPUs, this will happen over multiple cycles. For now, it happens in one cycle. After reading the instruction, we return a new state where the CPU is `ExecutingInstruction`. We also increment the program counter register `pc` by one.


\begin{code}
    LoadingInstruction -> (CPUState activity' registers', ram)
        where
        loadedWord = readRAM ram (pc registers)
        activity' = ExecutingInstruction (decodeInstruction loadedWord)
        registers' = registers {pc = increment (pc registers)}
\end{code}


If the CPU is currently `ExecutingInstruction`, we inspect the instruction and perform the appropriate action.

\begin{code}
    ExecutingInstruction instr -> case instr of
\end{code}

`LoadIm` is very simple; we just expand the 56-bit immediate value to 64 bits and store it in the register.

\begin{code}
        LoadIm reg val -> (CPUState LoadingInstruction registers', ram)
            where registers' = writeRegister registers reg (zeroExtend val)
\end{code}

`Add`, `Sub`, and `Mul` are all very similar:

\begin{code}
        Add a b d -> (CPUState LoadingInstruction registers', ram)
            where 
            result = readRegister registers a + readRegister registers b
            registers' = writeRegister registers d result
        Sub a b d -> (CPUState LoadingInstruction registers', ram)
            where 
            result = readRegister registers a - readRegister registers b
            registers' = writeRegister registers d result
        Mul a b d -> (CPUState LoadingInstruction registers', ram)
            where 
            result = readRegister registers a * readRegister registers b
            registers' = writeRegister registers d result
\end{code}

We don't have to actually implement integer addition, subtraction, and multiplication ourselves at the circuit level; these are pre-defined primitives in HDLs that hardware compilers know how to optimize aggressively.


For `Load` and `Store`, we will switch to the `ReadingMemory`/`WritingMemory` states. This isn't strictly necessary in our inefficient example hardware (we could perform a load or store in the same cycle as `ExecutingInstruction`, but I want to get us used to the idea of memory operations beign slow and requiring special consideration. We will perform the actual loads and stores in a separate step.


\begin{code}
        Load valReg ptrReg -> (CPUState (ReadingMemory ptr valReg) registers, ram)
            where 
            ptr = Ptr (readRegister registers ptrReg)
        Store valReg ptrReg -> (CPUState (WritingMemory ptr val) registers, ram)
            where 
            ptr = Ptr (readRegister registers ptrReg)
            val = Word (readRegister registers valReg)
\end{code}


For `Jmp`, we just need to modify the program counter. For `JmpZ`, we'll need to modify our program counter depending on whether or not the specified register is zero. Then, we need to go to the `LoadingInstruction` state.

\begin{code}
        Jmp destReg -> (CPUState LoadingInstruction registers', ram)
            where
            registers' = registers {pc = Ptr (readRegister registers destReg)}
        JmpZ zeroReg destReg -> (CPUState LoadingInstruction registers', ram)
            where
            registers' = if readRegister registers zeroReg == 0
                then registers {pc = Ptr (readRegister registers destReg)}
                else registers
\end{code}

For `Out`, we simply need to switch to the `Outputting` state.

\begin{code}
        Out reg -> (CPUState (Outputting output) registers, ram)
            where
            output = Output (readRegister registers reg)
\end{code}

For `Halt`, we switch to the `Halted` state.

\begin{code}
        Halt -> (CPUState Halted registers, ram)
\end{code}

That covers all the instructions! Now we just need to handle a few more possible CPU states.

For `ReadingMemory` and `WritingMemory`, we simply perform the requested RAM operation, write the new value, and go back to `LoadingInstruction`.

\begin{code}
    ReadingMemory ptr reg -> (CPUState LoadingInstruction registers', ram)
        where 
        Word ramValue = readRAM ram ptr
        registers' = writeRegister registers reg ramValue
    WritingMemory ptr val -> (CPUState LoadingInstruction registers, ram')
        where
        ram' = writeRAM ram ptr val
\end{code}

For `Outputting`, we simply stay in the `Outputting` state for a single cycle and then go back to `LoadingInstruction`. When we write the I/O hardware, we will actually output the value.


\begin{code}
    Outputting _ -> (CPUState LoadingInstruction registers, ram)
\end{code}

For `Halted`, we just stay `Halted` forever.

\begin{code}
    Halted -> (CPUState Halted registers, ram)
\end{code}


Now we'll write a couple functions to get information from the CPU state:

\begin{code}
isHalted :: CPUState -> Bool
isHalted (CPUState Halted _) = True
isHalted _                   = False

output :: CPUState -> Maybe Output
output (CPUState (Outputting output) _) = Just output
output _                                = Nothing
\end{code}

OK, now we're going to define how the CPU lives in hardware. Our CPU hardware is going to take some initial CPU state and emit an infinite stream of `Bool`s (for whether the CPU has halted) and `Maybe Output`s (for any outputs the CPU might emit).

\begin{code}
cpuHardware :: CPUState -> RAM -> Signal (Bool, Maybe Output)
cpuHardware initialCPUState initialRAM = outputSignal
    where
\end{code}

First, we're going to put the CPU state and RAM contents into a hardware register, which is usually implemented in hardware  as a D-type flip-flop. This is one of the unrealistic aspects I was talking about earlier; D-type flip-flops are expensive, so you wouldn't actually use them for RAM. However, they are very fast, which allows us to do the same-cycle RAM accesses this design uses.

\begin{code}
    systemState = register (initialCPUState, initialRAM) systemState'
\end{code}

Every cycle, we are going to replace the old `systemState` with a new state, which is defined as the `cycle` function applied to the old `systemState`.

\begin{code}
    systemState' = fmap cycle systemState
\end{code}

That part might seem a bit confusing because it's self-referential. Self-referential haskell code is how we make self-referential circuits, which is how you build hardware with memory. The output of the memory cells is being fed into an update function; at the beginning of every clock cycle, the memory cell updates itself with the output of the update function. 

```
 ---[cycle circuit]<---
|                      |
|                      |
|                      |
 --->[memory cell]-----
```

Now we have to take the state of the CPU, extract the relevant information from the state, and output that information.

\begin{code}
    getOutput :: (CPUState, RAM) -> (Bool, Maybe Output)
    getOutput (state, _) = (isHalted state, output state)
\end{code}

Every cycle, our output signal should contain that information.

\begin{code}
    outputSignal = fmap getOutput systemState'
\end{code}


Let's also define an initial CPU state with all registers (including `pc`) set to zero:

\begin{code}
defaultCPUState :: CPUState
defaultCPUState = CPUState LoadingInstruction (Registers 0 0 0 0 (Ptr 0))
\end{code}

OK, that's enough code for us to test our CPU in Haskell. Let's write a quick program in our assembly language:

\begin{code}
simpleProgram :: Vec 7 Instruction
simpleProgram = 
    LoadIm R1 7 :>
    LoadIm R2 8 :>
    LoadIm R3 9 :>
    Out R1      :>
    Out R2      :>
    Out R3      :>
    Halt        :>
    Nil
\end{code}

And let's compile it into memory:

\begin{code}
simpleProgramMem :: Vec 1024 Word
simpleProgramMem = fmap encode simpleProgram ++ repeat (Word 0)
\end{code}

Let's generate CPU hardware with this program pre-loaded:

\begin{code}
simpleProgramCPU :: Signal (Bool, Maybe Output)
simpleProgramCPU = cpuHardware defaultCPUState (RAM simpleProgramMem)
\end{code}

Because CLaSH is a wrapper around plain old haskell, we can test our entire CPU design without ever having to put it in hardware. Let's use the `sample` function to get a list of the first 20 outputs from our CPU, one output per clock cycle.

\begin{code}
simpleProgramOutput :: [(Bool, Maybe Output)]
simpleProgramOutput = take 20 $ sample simpleProgramCPU
\end{code}

Before we look at the output, let's take a moment to think about what we should expect.

The CPU starts out in `LoadingInstruction`, so it shouldn't output anything the first cycle. The second cycle, it executes the first `LoadIm` in the `ExecutingInstruction` state. Then, it goes back to `LoadingInstruction`. So each `LoadIm` takes 1 cycle to load and one cycle to execute. So we should first expect 5 cycles with no output.

Then, for each `Out` instruction, the CPU has to go through a `LoadingInstruction`, then an `ExecutingInstruction`, then an `Outputting` state. So for every `Out`, there should be two cycles with no output and one cycle with an output. So we should spend 9 cycles on `Out`s.

Then, for `Halt`, we have to go through `LoadingInstruction`, `ExecutingInstruction`, and then finally to `Halt`. So this should take a total of 3 instructions.

The total, then, is 17 instructions for this program to execute. On the 17th instruction, the halt bit should be `True`, and it should stay that way.

Indeed, if we type `mapM_ print simpleProgramOutput` at our interactive prompt, we get

```
(False,Nothing)
(False,Nothing)
(False,Nothing)
(False,Nothing)
(False,Nothing)
(False,Nothing)
(False,Nothing)
(False,Just (Output 7))
(False,Nothing)
(False,Nothing)
(False,Just (Output 8))
(False,Nothing)
(False,Nothing)
(False,Just (Output 9))
(False,Nothing)
(False,Nothing)
(True,Nothing)
(True,Nothing)
(True,Nothing)
(True,Nothing)
```

Which is exactly what we wanted!

Let's try a more complicated program, with a loop.

\begin{code}
loopProgram :: Vec 9 Instruction
loopProgram = 
    LoadIm R1 1  :> -- Constant 1
    LoadIm R2 5  :> -- Loop counter
    LoadIm R3 4  :> -- Jump destination ("Out R2")
    LoadIm R4 8  :> -- Jump destination ("Halt")
    Out R2       :> -- Output the current counter value
    JmpZ R2 R4   :> -- Halt if R2 == 0
    Sub R2 R1 R2 :> -- Decrement R2
    Jmp R3       :> -- Go to the start of the loop
    Halt         :>
    Nil

loopProgramMem :: Vec 1024 Word
loopProgramMem = fmap encode loopProgram ++ repeat (Word 0)

loopProgramCPU :: Signal (Bool, Maybe Output)
loopProgramCPU = cpuHardware defaultCPUState (RAM loopProgramMem)

-- Let's ignore boring outputs
isBoring :: (Bool, Maybe Output) -> Bool
isBoring (False, Nothing) = True
isBoring _                = False

loopProgramOutput :: [(Bool, Maybe Output)]
loopProgramOutput = take 7 $ filter (not . isBoring) $ sample loopProgramCPU
\end{code}

And if we print out the output:

```
(False,Just (Output 5))
(False,Just (Output 4))
(False,Just (Output 3))
(False,Just (Output 2))
(False,Just (Output 1))
(False,Just (Output 0))
(True,Nothing)
```

Great! Just what we wanted.

And let's make a program that puts the first 20 fibonacci terms in RAM. We'll use `R1` as a constant values register and `R2` as a pointer register.

\begin{code}
fibProgram :: Vec 27 Instruction
fibProgram
    =  LoadIm R1 0          -- Store the first 2 terms (0,1) in RAM
    :> LoadIm R2 0x100
    :> Store R1 R2
    :> LoadIm R1 1
    :> LoadIm R2 0x101
    :> Store R1 R2
    :> LoadIm R3 0          -- Loop counter
    :> LoadIm R2 0x100      -- Start of loop. Load fibonacci array base address
    :> Add R2 R3 R2         -- Get the address of the current first term (R2 + R3)
    :> Load R4 R2           -- Load the first item into R4
    :> LoadIm R1 1 
    :> Add R2 R1 R2         -- Get the address of the second item (R2 + R3 + 1)
    :> Load R1 R2           -- Load the second item into R1
    :> Add R4 R1 R4         -- Add up the first and second items
    :> LoadIm R1 1
    :> Add R2 R1 R2         -- Get the address of the new item (R2 + R3 + 2)
    :> Store R4 R2          -- Store the new item
    :> Out R4               -- Print the new item
    :> LoadIm R1 19
    :> Sub R1 R3 R1         -- R1 = 19 - loop count
    :> LoadIm R2 haltAddr   -- R2 = Halt address
    :> JmpZ R1 R2           -- Halt if R1 == 0 (i.e. if loop count is 19)
    :> LoadIm R1 1
    :> Add R3 R1 R3         -- Increment loop counter
    :> LoadIm R2 loopStart
    :> Jmp R2               -- Go back to loop start
    :> Halt
    :> Nil
    where
    haltAddr = 26
    loopStart = 7

fibProgramMem :: Vec 1024 Word
fibProgramMem = fmap encode fibProgram ++ repeat (Word 0)

fibProgramCPU :: Signal (Bool, Maybe Output)
fibProgramCPU = cpuHardware defaultCPUState (RAM fibProgramMem)


fibProgramOutput :: [(Bool, Maybe Output)]
fibProgramOutput = take 21 $ filter (not . isBoring) $ sample fibProgramCPU
\end{code}

And indeed, we get

```
(False,Just (Output 1))
(False,Just (Output 2))
(False,Just (Output 3))
(False,Just (Output 5))
(False,Just (Output 8))
(False,Just (Output 13))
...
(False,Just (Output 6765))
(False,Just (Output 10946))
(True,Nothing)
```

Sweet! 

OK, now for the final step in today's tutorial: putting this thing in actual hardware.

This part is pretty simple. First, we want to take the `Signal (Bool, Maybe Output)` stream from our CPU, which has an ambiguous/arbitrary bit-level representation, and transform it into a stream of something that has an unambiguous bit-level representation.

\begin{code}
hardwareTranslate :: (Bool, Maybe Output) -> (Bit, Bit, BitVector 64)
hardwareTranslate (halted, output) = (haltedBit, outputActive, outputValue)
    where
    haltedBit = if halted then 1 else 0
    (outputActive, outputValue) = case output of
        Nothing -> (0, 0)
        Just (Output val) -> (1, pack val)
\end{code}

Now we can apply this function to our stream of CPU output values to get a stream of bit values. If we call this stream of bit values `topEntity`, CLaSH knows that we want to compile this to a hardware object.

\begin{code}
topEntity :: Signal (Bit, Bit, BitVector 64)
topEntity = fmap hardwareTranslate fibProgramCPU
\end{code}

To compile the CPU, we just run `clash --verilog Main.lhs`.

This generates a bunch of `.v` files in `./verilog/Main`.

We use the following Verilog file (call it `cpu.v`) to run our CPU hardware:

```verilog
`timescale 1ps/1ps


module main();

    initial begin
        reset_reg = 0;
        reset_reg = #1 1;
    end

    // clock
    reg theClock = 1;
    wire clk;
    assign clk = theClock;
    always begin
        #500;
        theClock = !theClock;
    end

    reg reset_reg;
    wire reset = reset_reg;

    wire halt = 0;
    wire output_valid = 0;
    wire [63:0] output_data;

    % Main_topEntity evaluator(clk, reset, halt, output_valid, output_data);
    
    always@(posedge clk) begin
        if (output_valid == 1) begin
            $display("%H", output_data);
        end else begin
            $display(".");
        end
    end

    always@(posedge clk) begin
        if (halt == 1) $finish;
    end

endmodule
```

This prints the output data if there is any or a dot if there isn't any.

To compile the iverilog simulation, run

```bash
iverilog -o cpu cpu.v verilog/main/*.v
```

And to run it, run

```bash
timeout 5 ./cpu
```