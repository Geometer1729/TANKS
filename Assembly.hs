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

val :: Value -> Runtime -> Int
val (R "a") tam = let (a,_,_,_) = regs tam
                  in a
val (R "b") tam = let (_,b,_,_) = regs tam
                  in b
val (R "t") tam = let (_,_,t,_) = regs tam
                  in if t then 1 else 0
val (R "x") tam = let (_,_,_,x) = regs tam
                  in deref (mem tam) x
val (M i) tam = deref (mem tam) i
val X tam = let (_,_,_,x) = regs tam
            in deref (mem tam) x
val (V v) _ = v

data HardInst = Sit | Shoot | HScan Float Float | HAim Float | HMove | HGyro | HGPS

data Inst =   Load Value RegisterLabel
            | Add Value Value RegisterLabel
            | Sub Value Value RegisterLabel
            | Mul Value Value RegisterLabel
            | Div Value Value RegisterLabel
            | Mod Value Value RegisterLabel
            -- | XOR Value Value RegisterLabel
            -- | And Value Value RegisterLabel
            -- | Or  Value Value RegisterLabel
            | TLT Value Value
            | TEQ Value Value
            | Scan
            | Fire
            | Gyro
            | Move
            | Aim
            | GPS
            | Jmp Value
            | JmpIf Value
            | Nop

run :: Runtime -> (Runtime,HardInst)
run tam = let c = current tam
              memo = mem tam
              registers@(a,b,t,x) = regs tam
          in case (head c) of
            (Load v rl) -> (tam{regs=(load memo registers v rl),current = (tail c)},Sit)
            (Add v1 v2 rl) -> (tam{regs=(load memo registers (V (val v1 tam + val v2 tam)) rl),current=tail c},Sit)
            (Sub v1 v2 rl) -> (tam{regs=(load memo registers (V (val v1 tam - val v2 tam)) rl),current=tail c},Sit)
            (Mul v1 v2 rl) -> (tam{regs=(load memo registers (V (val v1 tam * val v2 tam)) rl),current=tail c},Sit)
            (Div v1 v2 rl) -> (tam{regs=(load memo registers (V (val v1 tam `div` val v2 tam)) rl),current=tail c},Sit)
            (Mod v1 v2 rl) -> (tam{regs=(load memo registers (V (val v1 tam `mod` val v2 tam)) rl),current=tail c},Sit)
            (TLT v1 v2) -> (tam{regs=(load memo registers (V (val v2 tam - val v1 tam)) "t"),current=tail c},Sit)
            (TEQ v1 v2) -> (tam{regs=(load memo registers (V (if val v1 tam == val v2 tam then 1 else 0)) "t"),current=tail c},Sit)
            Scan -> (tam,HScan ((fromIntegral a)*pi/128) ((fromIntegral b)*pi/128))
            Fire -> (tam,Shoot)
            Gyro -> (tam,HGyro)
            Move -> (tam,HMove)
            Aim -> (tam,HAim ((fromIntegral a)*pi/128))
            GPS -> (tam,HGPS)
            (Jmp vi) -> let v = val vi tam
                        in (tam{current=drop v (prog tam)},Sit)
            (JmpIf vi) -> let v = val vi tam
                          in if v > 0 then (tam{current=drop v (prog tam)},Sit) else (tam,Sit)

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
                        (M m) -> (a,b,t,deref mem m)
                        X -> (a,b,t,deref mem x)
                        (V i) -> (a,b,t,i)

setRegister :: Runtime -> Value -> RegisterLabel -> Runtime
setRegister tam v rl = let m = mem tam
                           reg = regs tam
                       in tam{regs=load m reg v rl}

deref :: Memory -> Int -> Int
deref = (!!)
