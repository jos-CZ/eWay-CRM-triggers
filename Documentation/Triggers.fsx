// USAGE: dotnet fsi Triggers.fsx <customer-dir>

#r "nuget: Fsharp.Data"

open System
open FSharp.Data

type TriggerDefinition = XmlProvider<Schema="../Triggers.xsd">

type Printer = string -> unit

let print (message:string) = Console.WriteLine(message)

let printWithColor color message =
  Console.ForegroundColor <- color
  print message
  Console.ResetColor()

let printBlue = printWithColor ConsoleColor.Blue
let printRed = printWithColor ConsoleColor.Red
let printYellow = printWithColor ConsoleColor.DarkYellow

let stringWithDefault (str:string) = Option.defaultValue str

let fullPath path =
  let p = IO.Path.GetFullPath(path).Replace('\\', '/')
  if p.EndsWith('/') then p.Remove(p.Length - 1) else p

let listFiles dir = seq {
  for file in IO.Directory.EnumerateFiles(dir, "*.xml", IO.SearchOption.AllDirectories) do
    yield fullPath file
}

let fail _ = failwith("USAGE: dotnet fsi Triggers.fsx <triggers-dir>")

if fsi.CommandLineArgs.Length <> 2 || not <| IO.Directory.Exists fsi.CommandLineArgs[1] then
  fail()

let customerDir = fullPath fsi.CommandLineArgs[1]

let printClass (printer:Printer) (c:TriggerDefinition.Class option) =
  match c with
  | None -> printRed "NOT A CLASS"
  | Some c -> printer $"  Class {c.Name} @ {c.Assembly}"

let printExecutable (printer:Printer) (e:TriggerDefinition.Executable option) =
  match e with
  | None -> printRed "NOT AN EXECUTABLE"
  | Some e -> printer $"  Executable {e.Target}"

let printProcedure (printer:Printer) searchDir (p:TriggerDefinition.StoredProcedure) =
  if IO.File.Exists $"{searchDir}/{p.Name}.sql" then
    let procDef = IO.File.ReadAllText $"{searchDir}/{p.Name}.sql"
    let docRE = Text.RegularExpressions.Regex($"eWaySP_SetProcedureDescription\s*'{p.Name}'\s*,\s*N?'(.*)'")
    let m = docRE.Match(procDef)
    printer $"""  {if m.Success then m.Groups[1].Value else "<documentation not found>"}"""
  printer $"  Stored Procedure: {p.Name}"

let printProcedures (printer:Printer) searchDir (p:TriggerDefinition.StoredProcedures option) =
  match p with
  | None -> printRed "NOT A STORED PROCEDURE"
  | Some p -> p.StoredProcedures |> Array.iter (printProcedure printer searchDir)

let jobCriterion (c:TriggerDefinition.Criterias) criterionName =
  match c.ActionCriterias |> Array.tryFind (fun c -> c.Name = criterionName) with
  | None -> $"{criterionName}: Not set"
  | Some v -> $"""{criterionName}: {Option.defaultValue "Not set" v.Value}"""

let printJobCriteria (printer:Printer) (c:TriggerDefinition.Criterias) =
  let jc = jobCriterion c
  printer $"""  Job: {jc "Time"} {jc "Repeat"} {jc "IterationCount"} {jc "Frequency"}"""

let criterion (c:TriggerDefinition.ActionCriteria) =
  match c.Operator with
  | Some "IsChanged" -> $"{c.Name} IsChanged"
  | Some op -> $"""{c.Name} {op} {Option.defaultValue "<not set>" c.Value}"""
  | None -> $"""{c.Name} Equals {Option.defaultValue "<not set>" c.Value}"""

let printCriteria (printer:Printer) (c:TriggerDefinition.Criterias) =
  let operator = Option.defaultValue "AND" c.Operator
  printer $"""  Criteria: {String.Join($" {operator} ", c.ActionCriterias |> Array.map criterion)}"""

let printTrigger searchDir (trigger:TriggerDefinition.TriggerDefinition) =
  let printer = if trigger.Active then print else printRed
  printer $"  Trigger type: {trigger.When}"
  printer $"""  Folder: {stringWithDefault "<no-folder>" trigger.Folder}"""
  match trigger.Action.Type with
  | "Class" -> printClass printer trigger.Action.Class
  | "Executable" -> printExecutable printer trigger.Action.Executable
  | "StoredProcedure" -> printProcedures printer searchDir trigger.Action.StoredProcedures
  | t -> failwith($"unknown action type: {t}")
  match trigger.Action.Criterias, trigger.When with
  | Some c, "ScheduledAtTime" -> printJobCriteria printer c
  | Some c, _ -> printCriteria printer c
  | None, _ -> printRed "  NO CRITERIA"
  print "  -------------"

print $"Scanning directory {customerDir}"

for file in listFiles customerDir do
  printYellow file
  try
    let subDirPart = IO.Path.GetDirectoryName(file).Replace('\\', '/').Substring(customerDir.Length + 1)
    printBlue subDirPart
  with
  | :? ArgumentOutOfRangeException -> printRed $"file not in subdirectory: {file}"
  let triggerXML = IO.File.ReadAllText file |> TriggerDefinition.Parse
  match triggerXML.Triggers with
  | None -> printRed $"no triggers in file {file}"
  | Some triggers -> triggers.TriggerDefinitions |> Array.iter (printTrigger (IO.Path.GetDirectoryName(file)))
