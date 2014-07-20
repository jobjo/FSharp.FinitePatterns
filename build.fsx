// include Fake lib
#r @"tools\FAKE\tools\FakeLib.dll"

open Fake

open System.Diagnostics
open System.IO

// Directories.
let buildDir  = @".\build\"
let testDir   = @".\test\"


// The clean target cleans the build and deploy folders
Target "Clean" (fun _ ->
    trace "Clean"
    CleanDirs ["./build/"; "./deploy/"]
)

Target "BuildLib" (fun _ ->
    trace "Build Lib"
    !! "src/*.fsproj"
    |> MSBuildRelease buildDir "Build"
    |> Log "AppBuild-Output: "
)

"Clean" ==> "BuildLib"

run "BuildLib"

