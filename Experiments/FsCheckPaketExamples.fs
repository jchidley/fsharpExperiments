// http://www.navision-blog.de/blog/2016/03/21/property-based-testing-in-the-real-world/
#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\FsCheck.2.10.3\lib\net452\FsCheck.dll"
    #r @"..\packages\FsCheck.Xunit.2.10.3\lib\net452\FsCheck.Xunit.dll"
    #r @"..\packages\xunit.runner.visualstudio.2.3.0\build\net20\..\_common\xunit.abstractions.dll"
    #r @"..\packages\xunit.assert.2.3.0\lib\netstandard1.1\xunit.assert.dll"
    #r @"..\packages\xunit.extensibility.core.2.3.0\lib\netstandard1.1\xunit.core.dll"
    #r @"..\packages\xunit.extensibility.execution.2.3.0\lib\net452\xunit.execution.desktop.dll"
#endif

namespace fSharpExperiments

module FsCheckPaketExamples =
    open Xunit
    open FsCheck
    open FsCheck.Xunit
/// Represents a NuGet package name
[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
[<CustomEquality; CustomComparison>]
type PackageName =
| PackageName of name:string * compareString:string
    
    member self.Name = 
       self |> function PackageName (name=n) -> n
    
    member self.CompareString = 
       self |> function PackageName (compareString=c) -> c

    override this.ToString() = 
        match this with
        | PackageName (name,_) -> name

    override this.Equals that = 
        match that with
        | :? PackageName as that -> this.CompareString = that.CompareString
        | _ -> false

    override this.GetHashCode () = hash this.CompareString

    interface System.IComparable with
       member this.CompareTo that = 
          match that with 
          | :? PackageName as that -> StringComparer.Ordinal.Compare(this.CompareString, that.CompareString)
          | _ -> invalidArg "that" "cannot compare value of different types"

/// Contains the version information.
[<CustomEquality; CustomComparison; StructuredFormatDisplay("{AsString}")>]
type SemVerInfo = 
    { /// MAJOR version when you make incompatible API changes.
      Major : uint32
      /// MINOR version when you add functionality in a backwards-compatible manner.
      Minor : uint32
      /// PATCH version when you make backwards-compatible bug fixes.
      Patch : uint32
      /// The optional PreRelease version
      PreRelease : PreRelease option
      /// The optional build no.
      Build : string
      BuildMetaData : string
      // The original version text
      Original : string option }
    
    member x.Normalize() = 
        let build = 
            if String.IsNullOrEmpty x.Build |> not && x.Build <> "0" then "." + x.Build
            else ""
                        
        let pre = 
            match x.PreRelease with
            | Some preRelease -> sprintf "-%s" preRelease.Origin
            | None -> ""

        sprintf "%d.%d.%d%s%s" x.Major x.Minor x.Patch build pre

    member x.NormalizeToShorter() = 
        let s = x.Normalize()
        let s2 = sprintf "%d.%d" x.Major x.Minor
        if s = s2 + ".0" then s2 else s

    override x.ToString() = 
        match x.Original with
        | Some version -> version.Trim()
        | None -> x.Normalize()
    
    member x.AsString
        with get() = x.ToString()

    override x.Equals(yobj) = 
        match yobj with
        | :? SemVerInfo as y -> 
            x.Major = y.Major && x.Minor = y.Minor && x.Patch = y.Patch && x.PreRelease = y.PreRelease && x.Build = y.Build && x.BuildMetaData = y.BuildMetaData 
        | _ -> false
    
    override x.GetHashCode() = hash (x.Minor, x.Minor, x.Patch, x.PreRelease, x.Build)
    interface System.IComparable with
        member x.CompareTo yobj = 
            match yobj with
            | :? SemVerInfo as y -> 
                if x.Major <> y.Major then compare x.Major y.Major
                else if x.Minor <> y.Minor then compare x.Minor y.Minor
                else if x.Patch <> y.Patch then compare x.Patch y.Patch
                else if x.Build <> y.Build then
                    match Int32.TryParse x.Build, Int32.TryParse y.Build with
                    | (true, b1), (true, b2) -> compare b1 b2
                    | _ -> compare x.Build y.Build
                else 
                    match x.PreRelease, y.PreRelease with
                    | None, None -> 0
                    | Some p, None -> -1
                    | None, Some p -> 1
                    | Some p, Some p2 when p.ToString() = "prerelease" && p2.ToString() = "prerelease" -> 0
                    | Some p, _ when p.ToString() = "prerelease" -> -1
                    | _, Some p when p.ToString() = "prerelease" -> 1
                    | Some left, Some right -> compare left right

            | _ -> invalidArg "yobj" "cannot compare values of different types"



    type PackageList = (PackageName*SemVerInfo) list
    type Dependency = PackageName * VersionRequirement
    type PackageGraph = (PackageName*SemVerInfo*(Dependency list)) list
    type ResolverPuzzle = PackageGraph * (Dependency list)