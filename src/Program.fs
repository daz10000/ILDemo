module IL.Main
// resources
// http://timjones.io/blog/archive/2014/09/13/writing-a-minic-to-msil-compiler-in-fsharp-part-5-code-generation


// Build a small demo to emit IL code
// We want a function that takes two integers and returns their sum
// We want to emit the IL code for this function

// We will use the System.Reflection.Emit namespace to emit the IL code
// We will use the System.Reflection namespace to load the emitted assembly and invoke the function

open System
open System.Reflection.Emit
open System.Reflection

let root = "c:/extproj/IL/emitted"
let emitAssembly (assemblyName:string) (assemblyBuilder:AssemblyBuilder) =

    // first a module
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName)
    let moduleName = moduleBuilder.FullyQualifiedName

    // let typeAttributes = TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Public
    let typeAttributes = TypeAttributes.Sealed ||| TypeAttributes.Public
    let typeBuilder = moduleBuilder.DefineType(moduleName + ".Program", typeAttributes)

    let methodBuilder =
        typeBuilder.DefineMethod("Main", MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [| typeof<int>  ; typeof<int>|])

    //let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName, assemblyName + ".dll")
    //let typeBuilder = moduleBuilder.DefineType("DemoType", TypeAttributes.Public)
    //let methodBuilder = typeBuilder.DefineMethod("Add", MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [| typeof<int>; typeof<int> |])
    let ilGenerator = methodBuilder.GetILGenerator()

    ilGenerator.Emit(OpCodes.Ldarg_0)
    ilGenerator.Emit(OpCodes.Ldarg_1)
    ilGenerator.Emit(OpCodes.Add)
    ilGenerator.Emit(OpCodes.Ret)

    let t = typeBuilder.CreateType()
    let main = typeBuilder.GetMethod("Main")
    // let x = assemblyBuilder.CreateInstance("demo")
    // x
    // Create an instance of the type

    let instance = Activator.CreateInstance(t)

    // Invoke the method
    printfn "Invoking method"
    let result = main.Invoke(instance, [| box 1; box 2 |])
    printfn "Result: %A" result
    ()

    // (t,main)


[<EntryPoint>]
let main argv =
    let assemblyName = "DemoAssembly"
    let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(AssemblyName(assemblyName), AssemblyBuilderAccess.Run)
    emitAssembly assemblyName assemblyBuilder
    0
