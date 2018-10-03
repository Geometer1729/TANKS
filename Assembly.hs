module Assembly where

data Runtime = Runtime {
                regs :: Register,
                mem :: Memory,
                prog :: [Inst],
                current :: [Inst]
              }

type RegisterLabel = String
type Register = (Int,Int,Int,Pointer)
{-
A register: stores 0-15
B register: stores 0-15
T register: stores 0 or 1
X register: stores a pointer to memory
-}
type Pointer = Int
type Memory = [Int] --Always length 64. Values overflow at 9
data Inst =  Assign Pointer Bool       --Sets memory location to value
           | Jump Int                  --Jumps to instruction in program
           | JumpIf RegisterLabel Int  --Jumps to instruction if register value is nonzero
           | Shoot                     --Fires the lazers
           | Scan                      {-Scans environment for enemies and friends.
                                       Puts number of enemies in A and number of friends in B-}
           | Move                      --Moves forward 10 units
           | Turn Int                  --Rotates by the number of degrees
           -- | TurnR RegisterLabel       --Rotates by value of register in degrees
           | Load Pointer RegisterLabel--Loads a value from memory into a register
           | Add Pointer               --Increments value in memory

--run :: Tank -> Runtime -> (Tank,Runtime)
--run = undefined
