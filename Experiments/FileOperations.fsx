#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\FsCheck\lib\net452\FsCheck.dll"
    #r @"..\packages\FsCheck.Xunit\lib\net452\FsCheck.Xunit.dll"
    #r @"..\packages\xunit.runner.visualstudio\build\_common\xunit.abstractions.dll"
    #r @"..\packages\xunit.assert\lib\netstandard1.1\xunit.assert.dll"
    #r @"..\packages\xunit.extensibility.core\lib\netstandard1.1\xunit.core.dll"
    #r @"..\packages\xunit.extensibility.execution\lib\net452\xunit.execution.desktop.dll"
#endif

open System.IO

// http://www.fssnip.net/pi/title/Recursively-find-all-files-from-a-sequence-of-directories
let rec allFiles dirs =
    if Seq.isEmpty dirs then Seq.empty else
        seq { yield! dirs |> Seq.collect Directory.EnumerateFiles
              yield! dirs |> Seq.collect Directory.EnumerateDirectories |> allFiles }

module investingFiles = 

    let renameFile filename =
        let fi = FileInfo(filename)
        let name = fi.FullName.Substring(0, fi.FullName.IndexOf(fi.Extension))
        // Used to be CreationTimeUtc but that doesn't work any more, so LastWriteTime I'm hoping is modification time
        let date = fi.LastWriteTimeUtc.ToString(" yyyy-MM-dd")
        (name + date + fi.Extension)

    let fullPath dir files :string list= 
        let combine dir file = 
            System.IO.Path.Combine(dir,file)
        files |> List.map (combine dir)

    let destinationDirectory = @"C:\Users\jackc\OneDrive\Documents\Investing"

    let fi = fullPath destinationDirectory ["jacksipp.csv";"jackisa.csv";"carolsipp.csv";"carolisa.csv"]

    let renamedFiles = fi |> List.map renameFile

    let downloadDir = @"S:\DadOnly\Downloads"
    let downloadFiles = ["portfolio-export.csv";"portfolio-export(1).csv";"portfolio-export(2).csv";"portfolio-export(3).csv"] 
                            |> fullPath downloadDir 

    let moveFile fl = 
        fl |> List.map (fun x -> File.Move(x, renameFile x))

    moveFile fi

    List.zip downloadFiles fi
    |> List.map (fun (x, y) -> File.Move(x, y))


