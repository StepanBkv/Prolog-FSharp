open System

let prime number = 
    let rec recur number del = 
        if number=1 then false
        else
        if number/2+1=del then true
        else
        if number%del=0 then false
        else recur number (del+1)
    recur number 2

let rec decompos number =
    let rec decomposmany number delprime prevdelprime count =
        if prime number then 
           if number = prevdelprime then count
           else
             count+1
        else  
           if prime delprime = true && number%delprime=0 then 
               if delprime=prevdelprime then decomposmany (number/delprime) delprime prevdelprime count
               else
                  decomposmany (number/delprime) delprime delprime (count+1)
           else
             decomposmany number (delprime+1) delprime count
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
        if count=0 then ();
        else
            Console.Write(number-count)
            Console.Write("  ")
            output number (count-1)

    Console.WriteLine("Введите n число простых множителей, из которых должны состоять n подряд идущих чисел: ");
    let simpleCount = Convert.ToInt32(Console.ReadLine());
    let result = perebor 2 simpleCount 0
    Console.WriteLine("Найденные числа: ")
    output result simpleCount
    Console.WriteLine()
    0

    //134043 134044 134045 1340