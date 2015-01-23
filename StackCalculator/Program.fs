open System

//let newStack = StackContents [1.0;2.0;3.0;]

//let (StackContents contents) = newStack

// Single case discriminated union
type Stack = StackContents of float list

// returns and empty stack
let EMPTY = StackContents []

let StackUnderFlow = (0.0, EMPTY)

//let push x aStack =
//    let (StackContents contents) = aStack
//    let newContents = x::contents
//    StackContents newContents

// refactored function of above
let push x (StackContents contents) =
    StackContents (x::contents)

let pop (StackContents contents) = 
    match contents with 
    | top::rest -> 
        let newStack = StackContents rest
        (top,newStack)
    | [] -> 
        failwith "Stack underflow"

//let ADD stack =
//    let x, s = pop stack // pop the top of the stack
//    let y, s2 = pop s    // pop the result stack
//    let result = x + y   // do the math
//    push result s2       // push back on the doubly-popped stack

//let MUL stack = 
//    let x, s = pop stack
//    let y, s2 = pop s
//    let result = x * y
//    push result s2

// move the duplication into a separate helper function
let binary mathFn stack =
    // pop the top of the stack
    let x, stack' = pop stack
    // pop the top of the stack again
    let y, stack'' = pop stack'
    // do the math
    let z = mathFn x y
    // push back on the doubly-popped stack
    push z stack''

// refactored function of above 
//let ADD aStack = binary (fun x y -> x + y ) aStack
// but we can eliminate the lambda as this is exactly what + is
//let ADD aStack = binary (+) aStack
// but we can partially apply the function
let ADD = binary (+)
let SUB = binary (-)
let MUL = binary (*)
let DIV = binary (/)

let unary f stack =
    let x, stack' = pop stack   // pop the top of the stack
    push (f x) stack'           // push the function value on the stack

let NEG = unary (fun x -> -x)
let SQUARE = unary (fun x -> x * x)

let SHOW stack =
    let x, _ = pop stack
    printfn "The answer is %f" x
    stack  // keep going with the same stack

/// Duplicate the top value on the stack
let DUP stack = 
    // get the top of the stack
    let x,_ = pop stack  
    // push it onto the stack again
    push x stack 

/// Swap the top two values
let SWAP stack = 
    let x,s = pop stack  
    let y,s' = pop s
    push y (push x s') 

/// Make an obvious starting point
let START = EMPTY

[<EntryPoint>]
let main argv =
    let emptyStack = StackContents []
    let stackWith1 = push 1.0 emptyStack
    let stackWith2 = push 2.0 stackWith1

//    infers that stack is of type Stack of StackContents and returns a Stack of StackContents
//    let ONE stack = push 1.0 stack
//    let TWO stack = push 2.0 stack
    
//    these will return partially applied functions of (Stack -> Stack)
    let ONE = push 1.0
    let TWO = push 2.0
    let THREE = push 3.0
    let FOUR = push 4.0
    let FIVE = push 5.0

    let stackWith1 = ONE EMPTY // passes and empty stack to be applied
    let stackWith2 = TWO stackWith1 // passes a stack with contents to be applied
    let stackWith3 = THREE stackWith2

    // remove the intermediate stacks and apply the result to the next function via forward pipe
    let result123 = EMPTY |> ONE |> TWO |> THREE
    let result321 = EMPTY |> THREE |> TWO |> ONE

    // test pop
    let initialStack = EMPTY |> ONE |> TWO
    let popped1, poppedStack = pop initialStack
    let popped2, poppedStack2 = pop poppedStack

//    let _ = pop EMPTY // test underflow

    // test ADD and MUL
    let add1and2 = EMPTY |> ONE |> TWO |> ADD
    let add2and3 = EMPTY |> TWO |> THREE |> ADD
    let mult2and3 = EMPTY |> TWO |> THREE |> MUL

    // test DIV and SUB
    let div2by3 = EMPTY |> THREE|> TWO |> DIV
    let sub2from5 = EMPTY  |> TWO |> FIVE |> SUB
    let add1and2thenSub3 = EMPTY |> ONE |> TWO |> ADD |> THREE |> SUB

    // test NEG and SQUARE
    let neg3 = EMPTY  |> THREE|> NEG
    let square2 = EMPTY  |> TWO |> SQUARE

    let showResult = EMPTY |> ONE |> THREE |> ADD |> TWO |> MUL |> SHOW

    START |> ONE |> TWO |> SHOW |> ignore

    START
    |> ONE |> TWO |> ADD |> SHOW 
    |> THREE |> ADD |> SHOW 
    |> ignore

    START |> THREE |> DUP |> DUP |> MUL |> MUL |> ignore // 27

    START
    |> ONE |> TWO |> ADD |> SHOW  // 3
    |> THREE |> MUL |> SHOW       // 9
    |> TWO |> SWAP |> DIV |> SHOW // 9 div 2 = 4.5
    |> ignore


    // building new functions with composition

    // define a new function
    let ONE_TWO_ADD = 
        ONE >> TWO >> ADD 

    // test it
    START |> ONE_TWO_ADD |> SHOW |> ignore

    // define a new function
    let SQUARE = 
        DUP >> MUL 

    // test it
    START |> TWO |> SQUARE |> SHOW |> ignore

    // define a new function
    let CUBE = 
        DUP >> DUP >> MUL >> MUL 

    // test it
    START |> THREE |> CUBE |> SHOW |> ignore

    // define a new function
    let SUM_NUMBERS_UPTO = 
        DUP                     // n  
        >> ONE >> ADD           // n+1
        >> MUL                  // n(n+1)
        >> TWO >> SWAP >> DIV   // n(n+1) / 2 

    // test it
    START |> THREE |> SQUARE |> SUM_NUMBERS_UPTO |> SHOW |> ignore

    Console.ReadLine() |> ignore
    0 // return an integer exit code
