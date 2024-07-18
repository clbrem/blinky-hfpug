open System
open System.IO
open System.Text.Json

/// This is a program that imitates the toil of an AC repairman trying to read error codes from a control board.
/// In this case, there are two fuses represented by text files: fuse0.txt and fuse1.txt.
/// If the file is missing, then the corresponding fuse is "missing"
/// The fuse should contain some json content. If the fuse has json content {"status": true}, then the fuse is operational.
/// Otherwise the fuse is broken!

/// There are two failure modes: either the fuse is missing or the fuse is broken. We are using a bool to represent fuse 0 or fuse 1
type Failures =
    | FuseMissing of bool    
    | FuseBroken of bool

// An error code is a translation of the failure mode into a list of booleans. True is a "long" blink, False is a "short" blink
type ErrorCode =
    bool list
    
// A Fuse is a text file that contains some Json. The JSON should deserialize to a record with a boolean status field    
[<CLIMutable>]
type Fuse = {
    status: bool
}


// The exceptions! we use these to represent the failure modes. Exceptions are eager: they crop up when
// something goes wrong, so we can handle one error at a time.

type FuseBrokenException( fuse: bool) =
    inherit Exception("Fuse is Broken")
    member this.Fuse = fuse
type FuseMissingException( fuse: bool) =
    inherit Exception("Fuse is Missing")
    member this.Fuse = fuse

module ErrorCode =
    let private clearLine() =
        let currentLineCursor = Console.CursorTop
        Console.SetCursorPosition(0, currentLineCursor)
        Console.Write (String.init Console.WindowWidth (fun _ -> " "))
        Console.SetCursorPosition(0, currentLineCursor)
    let blink(s: int) =
        async {        
            Console.Write("   *   ")
            do! Async.Sleep s
            clearLine()
            do! Async.Sleep 500
        }
    
    let blinkShort = blink 500
    let blinkLong = blink 1500
    let render (ec: ErrorCode) =
        let rec render acc = function
            | [] -> acc 
            | true::xs -> render (acc + "L") xs
            | false::xs ->  render (acc + "S") xs
        render "" ec
    let blinkCode (ec: ErrorCode) =
        async {
            for err in ec do
                if err then do! blinkLong
                else do! blinkShort
        }
    

module Failures =
    
    let toErrorCode =
        function
        | FuseMissing true -> [true; false; false; true]
        | FuseMissing false -> [true; false; false; false]
        | FuseBroken true -> [true; true; false; true]
        | FuseBroken false -> [true; true; false; false]
    let description =
        function
        | FuseMissing fuse -> $"Fuse {if fuse then 1 else 0 } is missing"        
        | FuseBroken fuse -> $"Fuse {if fuse then 1 else 0} is broken"
    let print() =
        for failure in [FuseMissing true; FuseMissing false; FuseBroken true; FuseBroken false] do
            Console.WriteLine($"{failure |> toErrorCode |> ErrorCode.render} : {description failure}")

module Fuse =
    let readFuse (i:int) =
        
          try
            let fuse = File.ReadAllText $"fuse{i}.txt" |> JsonSerializer.Deserialize<Fuse>
            if (not fuse.status) then raise (FuseBrokenException((i = 1)))
          with        
          | :? FileNotFoundException as e ->
              FuseMissingException((i=1)) |> raise
              
          | :? System.Text.Json.JsonException as e ->
              FuseBrokenException((i=1)) |> raise

            
[<EntryPoint>]
let main argv =
    match argv |> List.ofArray with
    | ("--codes" | "-c") :: _ ->
        Failures.print()
    | _ -> 
       (
        async {
             try              
               do Fuse.readFuse 0
               do Fuse.readFuse 1
             with
             | :? FuseBrokenException as e ->
                 return! FuseBroken (e.Fuse) |> Failures.toErrorCode |> ErrorCode.blinkCode  
             | :? FuseMissingException as e ->
                  return! FuseMissing (e.Fuse) |> Failures.toErrorCode |> ErrorCode.blinkCode
         } |> Async.StartAsTask
        ).GetAwaiter().GetResult()            
    
    0

    
