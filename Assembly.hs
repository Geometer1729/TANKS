module Assembly where

data Runtime = Runtime {
                regs :: Register,
                mem :: Memory,
                prog :: [Inst],
                current :: [Inst]
              }

type Memory = [Int]
type RegisterLabel = String
type Register = (Int,Int,Bool,Int)

data Value = R RegisterLabel | M Int | X

data Inst =   Load Value RegisterLabel
            | Add Value Value RegisterLabel
            | Sub Value Value RegisterLabel
            | Mul Value Value RegisterLabel
            | Div Value Value RegisterLabel
            | Mod Value Value RegisterLabel
            | TLT Value Value
            | TEQ Value Value
            | Scan
            | Fire
            | Gyro
            | Move
            | GPS
            | Jmp Value
            | JmpIf Value
            | Nop
