{-# LANGUAGE LambdaCase #-}

module Assembly where

import HTank

data Runtime = Runtime {
                regs :: Register,
                mem :: Memory,
                prog :: [Inst],
                current :: [Inst]
              }

type Memory = [Int]
type RegisterLabel = String
type Register = (Int,Int,Bool,Int)

data Value = R RegisterLabel | M Int | X | V Int

data Inst =   Load Value RegisterLabel
            | Add Value Value RegisterLabel
            | Sub Value Value RegisterLabel
            | Mul Value Value RegisterLabel
            | Div Value Value RegisterLabel
            | Mod Value Value RegisterLabel
            | XOR Value Value RegisterLabel
            | And Value Value RegisterLabel
            | Or  Value Value RegisterLabel
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

run :: Runtime -> (Runtime,HardInst)
run tam = let c = current tam
              memo = mem tam
              registers = regs tam
          in case (head c) of
            (Load v rl) -> (tam{regs=(load memo registers v rl),current = (tail c)},Sit)

load :: Memory -> Register -> Value -> RegisterLabel -> Register
load mem (a,b,t,x) v "a" = case v of
                        (R rl) -> case rl of
                                    "a" -> (a,b,t,x)
                                    "b" -> (b,b,t,x)
                                    "t" -> (if t then 1 else 0,b,t,x)
                                    "x" -> (deref mem x,b,t,x)
                        (M m) -> (deref mem m,b,t,x)
                        X -> (deref mem x,b,t,x)
                        (V i) -> (i,b,t,x)
load mem (a,b,t,x) v "b" = case v of
                        (R rl) -> case rl of
                                    "a" -> (a,a,t,x)
                                    "b" -> (a,b,t,x)
                                    "t" -> (a,if t then 1 else 0,t,x)
                                    "x" -> (a,deref mem x,t,x)
                        (M m) -> (a,deref mem m,t,x)
                        X -> (a,deref mem x,t,x)
                        (V i) -> (a,i,t,x)
load mem (a,b,t,x) v "t" = case v of
                        (R rl) -> case rl of
                                    "a" -> (a,b,a > 0,x)
                                    "b" -> (a,b,b > 0,x)
                                    "t" -> (a,b,t,x)
                                    "x" -> (a,b,deref mem x > 0,x)
                        (M m) -> (a,b,deref mem m > 0,x)
                        X -> (a,b,deref mem x > 0,x)
                        (V i) -> (a,b,i > 0,x)
load mem (a,b,t,x) v "x" = case v of
                        (R rl) -> case rl of
                                    "a" -> (a,b,t,a)
                                    "b" -> (a,b,t,b)
                                    "t" -> (a,b,t,if t then 1 else 0)
                                    "x" -> (a,b,t,deref mem x)


deref:: Memory -> Int -> Int
deref = (!!)
