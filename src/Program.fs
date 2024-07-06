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

let program1() =
    printfn "Build a simple program"

    // pieces needed for a main function
    // params, localdefs, statements
    let parameters : Parameters = [ (ScalarVariableDeclaration(Int, "a")); (ScalarVariableDeclaration(Int, "b")) ]
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
        let e3 = IdentifierExpression({Identifier = "c"})
        ReturnStatement (Some e3)
    ]
    let mainDecl = FunctionDeclaration(Int, "main", parameters, (localDefs, mainStatements))

    // Overall program is just our main for now
    let program : Declaration list = [
        mainDecl
    ]
    program

let program2() =
    // -----------------------------------------
    // Demo a static global variable and two functions
    // -----------------------------------------
    // one that can set it and another than can use it and add one to it
    // roughly equivalent to
    // -----------------------------------------
    // let mutable global1 = 0
    // let setGlobal1() = global1 <- 1
    // let getGlobal1AndAdd1() = global1 + 1
    // let main() = setGlobal1(); let x = getGlobal1AndAdd1(); return x
    // -----------------------------------------

    printfn "program2: Build a simple program"

    let globalStatic1 = Declaration.StaticVariableDeclaration(ScalarVariableDeclaration(Int, "global1"))

    let setGlobal1Statements : Statement list = [
        // global1 = 1
        let e = Expression(ScalarAssignmentExpression({Identifier = "global1"}, LiteralExpression(Literal.IntLiteral 1)))
        ExpressionStatement(e)
    ]

    let getGlobal1AndAdd1Statements : Statement list = [
        // return global1 + 1
        let e = BinaryExpression(IdentifierExpression({Identifier = "global1"}), Add, LiteralExpression(Literal.IntLiteral 1))
        ReturnStatement(Some e)
    ]


    let setGlobal1 = FunctionDeclaration(Void, "setGlobal1", [], ([], setGlobal1Statements))

    let getGlobal1AndAdd1 = FunctionDeclaration(Int, "getGlobal1AndAdd1", [], ([], getGlobal1AndAdd1Statements))

    let mainStatements = [
        // call setGlobal1()
        let e = FunctionCallExpression("setGlobal1", [])
        ExpressionStatement(Expression(e))

        // let x = getGlobal1AndAdd1()
        let e2 = FunctionCallExpression("getGlobal1AndAdd1", [])
        let vRef = {Identifier = "x"}
        let e3 = Expression(ScalarAssignmentExpression(vRef, e2))
        ExpressionStatement(e3)

        // return x
        let e4 = IdentifierExpression({Identifier = "x"})
        ReturnStatement(Some e4)
    ]
    let mainLocals : LocalDeclarations = [ ScalarVariableDeclaration(Int, "x")]

    let mainDecl = FunctionDeclaration(Int, "main", [], (mainLocals, mainStatements))

    // Overall program is just our main for now
    let program : Declaration list = [
        globalStatic1 // static var
        setGlobal1 // func to set
        getGlobal1AndAdd1 // func to get and add
        mainDecl
    ]
    program


[<EntryPoint>]
let main argv =
    let program = program2()

    // Incantations to analyze, build and emit the IL code
    let sa = SemanticAnalysis.analyze program
    let builder = ILBuilder(sa)
    let theClass = builder.BuildClass program

    // emit the symbolic source
    // This dumps the human readable IL code
    let ilSrc = theClass.ToString()
    printfn $"IL source: {ilSrc}"

    // emit the IL code to a DLL
    let assemblyName = "DemoAssembly"
    let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(AssemblyName(assemblyName), AssemblyBuilderAccess.Run)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName)
    let codeGen = CodeGenerator(moduleBuilder,theClass,"DemoModule")
    let (compiledType, entryPoint) = codeGen.GenerateType()
    let instance = Activator.CreateInstance(compiledType)

    // ------------------------------------------------------
    // call entry point and pass in arguments
    // ------------------------------------------------------

    // program 1
    //let result = entryPoint.Invoke(instance, [| box 1; box 2 |])
    let result = entryPoint.Invoke(instance, [| |])
    // show result
    printfn $"Result: {result}"
    0
