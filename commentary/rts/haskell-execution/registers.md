# Haskell Excecution: Registers



Source files: [includes/stg/Regs.h](/trac/ghc/browser/ghc/includes/stg/Regs.h), [includes/stg/MachRegs.h](/trac/ghc/browser/ghc/includes/stg/MachRegs.h)



During execution of Haskell code the following (virtual) registers are always valid:


- `Hp` points to the byte before the first free byte in the (contiguous) allocation space.

- `HpLim` points to the last available byte in the current chunk of allocation space.

- `Sp` points to the youngest allocated byte of stack.  The stack grows downwards.  Why?  Because that means a return address is at a lower address than the stack frame it "knows about", and that in turn means that we can treat a stack frame very like a heap object, with an info pointer (return address) as its first word.

- `SpLim` points to the last (youngest) available byte in the current stack.


There are bunch of other virtual registers, used for temporary argument passing, for words, floats and doubles: `R1` .. `R10`, `F1` .. `F4`, `D1` .. `D4`, `L1` .. `L2`.



In a register-rich machine, many of these virtual registers will be mapped to real registers.  In a register-poor machine, they are instead allocated in a static memory record, pointed to by a real register, `BaseReg`.



The code generator knows how many real registers there are, and tries to avoid using virtual registers that are not mapped to real registers.  So, for example, it does not use `R5` if the latter is memory-mapped; instead, it passes arguments on the stack.


