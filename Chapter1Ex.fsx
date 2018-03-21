/// Exercise 1 ///

/// generates random int such low <= x < high
let randomInt = 
    let rnd = System.Random()
    (fun low high -> low + rnd.Next() % (high - low) )


let swap (left : 'T byref) (right : 'T byref) =
    let temp = left
    left <- right
    right <- temp

let printArray (arr : 'T []) = 
    System.Console.Write("[|")
    for i in 0 .. arr.Length - 1 do
        System.Console.Write(arr.[i])
        System.Console.Write("; ")
    System.Console.Write("|]\n")


// select k-th element of an array
let select array k =
    // place the element at i-th position at its relative position low <= k < high
    // such that for every j < k arr[j] <= arr[k] and for every k < j arr[k] <= arr[j])
    let arrayCopy = Array.copy array
    let placeItem (arr: 'T []) i low high = 
        let move p k = 
            swap (&arr.[k]) (&arr.[p+1])
            swap (&arr.[p]) (&arr.[p+1])
        let rec tryMove p k = 
            if k <= p || high <= k || p < low then p
            else if arr.[p] > arr.[k] then 
                    move p k
                    tryMove (p+1) (k+1)
             else tryMove p (k+1)
        swap (&arr.[i]) (&arr.[low])
        tryMove low (low+1)
    let rec find arr i low high =
        let pivot = randomInt low high
        let res = placeItem arr i low high
        if res = i then arr.[i]
        elif res < i then find arr i (res+1) high
        else find arr i low res
    find arrayCopy k 0 arrayCopy.Length