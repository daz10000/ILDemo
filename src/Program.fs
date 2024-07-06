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
open IL

(*
// simple standalone function to emit IL code
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

let main1 argv =
    let assemblyName = "DemoAssembly"
    let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(AssemblyName(assemblyName), AssemblyBuilderAccess.Run)
    emitAssembly assemblyName assemblyBuilder
    0
*)

open IL.Ast

[<EntryPoint>]
let main argv =
    printfn "Build a simple program"
    // TypeSpec * Identifier * Parameters * CompoundStatement
    let parameters : Parameters = [ (ScalarVariableDeclaration(Int, "a")); (ScalarVariableDeclaration(Int, "b")) ]

    // and CompoundStatement = LocalDeclarations * Statement list

    // and LocalDeclarations = VariableDeclaration list
    let localDefs : LocalDeclarations = [ ScalarVariableDeclaration(Int, "c")]
    let mainStatements : Statement list = [
        // a+b
        let e =
                BinaryExpression(
                    IdentifierExpression(
                        {Identifier = "a"}),
                    Add,
                    IdentifierExpression({Identifier = "b"})
                )
        let vRef = {Identifier = "c"}
        // c = a + b
        let e2 = Expression(ScalarAssignmentExpression(vRef, e))
        ExpressionStatement(e2)

        // c = c * 2
        ExpressionStatement(
            Expression(
                ScalarAssignmentExpression(
                    {Identifier = "c"},
                    BinaryExpression(
                        IdentifierExpression({Identifier = "c"}),
                        Multiply,
                        LiteralExpression(
                            Literal.IntLiteral 2
                        )
                    )
                )
            )
        )

        // return statement
        // ReturnStatement(Some(IdentifierExpression(vRef)))
        let e3 = IdentifierExpression({Identifier = "c"})
        ReturnStatement (Some e3)
    ]
    let mainDecl = FunctionDeclaration(Int, "main", parameters, (localDefs, mainStatements))

    let program : Declaration list = [
        mainDecl
    ]

    let sa = SemanticAnalysis.analyze program
    let builder = ILBuilder(sa)
    let theClass = builder.BuildClass program

    // emit the symbolic source
    let ilSrc = theClass.ToString()
    printfn $"IL source: {ilSrc}"

    // emit the IL code
    let assemblyName = "DemoAssembly"
    let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(AssemblyName(assemblyName), AssemblyBuilderAccess.Run)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName)
    let codeGen = CodeGenerator(moduleBuilder,theClass,"DemoModule")
    let (compiledType, entryPoint) = codeGen.GenerateType()
    let instance = Activator.CreateInstance(compiledType)

    // call entry point and pass in arguments
    let result = entryPoint.Invoke(instance, [| box 1; box 2 |])
    // show result
    printfn $"Result: {result}"
    0
