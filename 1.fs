module s_1

open System


let ex1 = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet""".Split("\n")

let ex2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen""".Split("\n")


let realInput = System.IO.File.ReadAllLines "inputs/01.txt"

let digitMap = Map [ 
    ("one", 1); 
    ("two", 2); 
    ("three", 3); 
    ("four", 4); 
    ("five", 5); 
    ("six", 6); 
    ("seven", 7); 
    ("eight", 8); 
    ("nine", 9)
]

let charSeqToString = Seq.toArray >> String

let processLine (line: string) =
    let digits = line |> Seq.filter Char.IsDigit
    $"{digits |> Seq.head}{digits |> Seq.last}" |> int

let solveFirst () =
    realInput |> Seq.map processLine |> Seq.sum

let getLeftWindows(line: string) =
    Seq.init line.Length (fun x -> line |> Seq.take (x+1) |> charSeqToString)

let getRightWindows(line: string) =
    Seq.init line.Length (fun x -> line |> Seq.skip (line.Length - x - 1) |> charSeqToString)

let isStringNumeric(value: string) :  int option =
    let digit = value |> Seq.tryFind Char.IsDigit |> Option.map (fun x -> $"{x}" |> int)
    digit |> Option.orElse (digitMap |> Map.toSeq |> Seq.tryFind (fun (x, _) -> value.Contains(x)) |> Option.map snd)

let processLineSecond (line: string) =
    let left = line |> getLeftWindows |> Seq.tryPick isStringNumeric
    let right = line |> getRightWindows |> Seq.tryPick isStringNumeric
    match left, right with
    | Some l, Some r -> ($"{l}{r}" |> int)
    | _ -> failwith "No digit"

let solveSecond () =
    realInput |> Seq.map processLineSecond |> Seq.sum