open System

let prime n = 
    let rec recur n m = 
        if(n=1) then false
        else
        if(n/2+1=m) then true
        else
        if(n%m=0) then false
        else recur n (m+1)
    recur n 2

let rec decompos number =
    let rec decomposmany number delitel previous count =
        if(delitel>number && number<>1) then -1
            else
                if(prime number) then 
                    if (number = previous) then count
                        else
                            count+1
                else 
                    if(number=1) then count
                        else
                            if(prime delitel = true && number%delitel=0) then 
                                if (delitel=previous) then decomposmany (number/delitel) delitel previous (count)
                                    else
                                        decomposmany (number/delitel) delitel delitel (count+1)
                            else
                                decomposmany number (delitel+1) delitel count
    decomposmany number 2 1 0

let rec perebor number limit count =
    if(count=limit) then number;
        else
            let temp = decompos number 
            if (temp = limit) then perebor (number+1) limit (count+1)
                else
                    perebor (number+1) limit 0

[<EntryPoint>]
let main argv =
    let rec output number count=
        if(count=0) then ();
        else
            Console.Write(number-count)
            Console.Write("  ")
            output number (count-1)

    Console.WriteLine("Введите n число простых множителей, из которых должны состоять n подряд идущих чисел");
    let simpleCount = Convert.ToInt32(Console.ReadLine());
    let result = perebor 2 simpleCount 0
    Console.WriteLine("Найденные числа: ")
    output result simpleCount
    Console.WriteLine()
    0