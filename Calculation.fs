module Calculation
open System
open System.IO
open FSharp.Data

    module Analytical =
    
        (** SOME GENERAL FUNCTIONS **)
        // Integral - needed for calculating PHI(x) function which is integral from -inf to x
        let SimpsonIntegral (f) (a:float) (b:float) (n:float) = // from a to b; 'a' cannot be -inf
            let dx = (b-a)/n
            // printfn "%f" dx
            let rec Integral f (a:float) (b:float) (dx:float) (result:float) = 
                if ( a > (b  - dx) ) then result
                else Integral (f) (a+dx) (b) (dx) (result + ((a+dx) - a)/6. * (f(a) + 4.*f((a+(a+dx))/2.) + f(a+dx)))
            Integral f a b dx 0.


        // Mapping function with other parameters as constants
        let myMap (func) (A:float list) (x:float) (y:float) (z:float) (v:float) =
            let myfunc2 (a:float) (x:float) (y:float) (z:float) (v:float)=
                func a x y z v

            let myfunc3 (a:float) = 
                myfunc2 a x y z v

            A |> List.map myfunc3 

        // Zipping four lists together
        let myZip (A:float list) (B:float list) (C:float list) (D:float list)=
            let rec myhelpZip2 (A:float list) (B:float list) (C:float list) (D:float list) (result:float list list) =
                if A.IsEmpty then result[1..]
                else  myhelpZip2 (A.Tail) (B.Tail) (C.Tail) (D.Tail) (result @ [[A.Head] @ [B.Head] @ [C.Head] @ [D.Head]])
            myhelpZip2 A B C D [[]]

        (** SOME GENERAL FUNCTIONS END **)

        // First create function to calculate parameter d1
        let d1 (S0:float) (K:float) (r:float) (T:float) (sigma:float) = (log(S0/K)) + (r + (sigma * sigma)/2.) * T / (sigma * sqrt(T))

        // Second create function PHI
        (** this one is trickey because I should integrate from -INF to x
            but that is not possible here, at least using implemented functions
            analizing the function under integral show that 
            for x<-4 -> PHI(x)<0.001 and getting smaller rapidly
            with small error (compared to WolframAlpha same answer) 
            this function is getting -inf as -100+d1 for d1<0 and -100 otherwise
        **)
        let phiFunc (d1:float) = 
            if d1<0 then 
                1. / sqrt(2. * Math.PI) * SimpsonIntegral (fun x -> exp(-x * x / 2.)) (-100.+d1) d1 100000
            else 
                1. / sqrt(2. * Math.PI) * SimpsonIntegral (fun x -> exp(-x * x / 2.)) (-100.) d1 100000

        // Third from PHI we can obtain deltas - which are PHI functions
        let delta_call (S0:float) (K:float) (r:float) (T:float) (sigma:float) =
            d1 S0 K r T sigma|> phiFunc
        let delta_put (S0:float) (K:float) (r:float) (T:float) (sigma:float) =
            -((d1 S0 K r T sigma)*(-1.) |> phiFunc)

        // Four create BS formula using above functions
        let BScall (phiFunc) (d1) (S0:float) (K:float) (r:float) (T:float) (sigma:float) = 
            let d1_val = d1 S0 K r T sigma
            S0 * phiFunc(d1_val) - K * exp(-r*T) * phiFunc(d1_val - sigma*sqrt(T))

        let BSput (phiFunc) (d1) (S0:float) (K:float) (r:float) (T:float) (sigma:float) = 
            let d1_val = d1 S0 K r T sigma
            -S0 * phiFunc(-d1_val) + K * exp(-r*T) * phiFunc(-d1_val + sigma*sqrt(T))


        (**HOW TO USE: 
        d1 S0 K r T sigma
        phiFunc(d1 S0 K r T sigma)
        delta_call S0 K r T sigma
        delta_put S0 K r T sigma
        BScall phiFunc d1 S0 K r T sigma
        BSput phiFunc d1 S0 K r T sigma
        **)
    ;;
    (*####################################################################################################################*)

    module MonteCarlo = 
        let seed:int = 2023

        module Random =
            let rand = new System.Random(seed)
            // draw two random variables
            let randU1U2 ()  = (rand.NextDouble(), rand.NextDouble())
            // using Box-Muller transformation make two normal distributed variables
            let BoxMuller (num1, num2) =
                let N1 = sqrt(-2. * log num1) * sin (2. * Math.PI * num2)
                let N2 = sqrt(-2. * log num1) * cos (2. * Math.PI * num2)
                (N1, N2) 

            let N1_N2 = randU1U2 >> BoxMuller
            

        module European = 
            // lets make k - used once for creating random variables
            let k n :int = Convert.ToInt32(Math.Ceiling(n/2.))
            // consts - constant values in Simulation of Geometric Brownian Motion (to not calculate them over again)
            let consts n r o t = ((r-o*o/2.)*t/n, o*sqrt(t/n))

            // Z - is a list of are two independent standard normal random variables
            let listZpom k = [ for i in 1 .. k ->Random.N1_N2() ]
            let Zi listZpom = List.map (fun (x,y) -> x) listZpom @ List.map (fun (x,y) -> y) listZpom
            let Z = k >> listZpom >> Zi // Random variables used in Geometric Brownian Motion

            // Si - recursive function defined in Geometric Brownian Motion
            let rec Si (i:int) (S0:float) (constOne:float, constTwo:float) (Z:float list) = 
                match i with
                | 0 -> S0 
                | _ -> Si (i-1) (S0) (constOne, constTwo) (Z) * exp(constOne + constTwo * Z[i])

            // S - for convenience, pass just initial arguments
            let S i n S0 r o t =   Si (i) (S0) (consts n r o t) (Z n) 

            // listSi - list of S for one path helpful with calculation of Ri
            let listSi (n:float) S0 r o t  = [for i in 1 .. Convert.ToInt32(n) -> S (i-1) n S0 r o t ]

            let BScall_MC (N:int) (n:float) (S0:float) (K:float) (r:float) (o:float) (t:float) = 
                let L = [for i in 1 .. N -> (listSi n S0 r o t)[Convert.ToInt32(n)-1] ]
                let sum_payoff = List.sumBy (fun elem -> if elem > K then elem - K else 0) L
                exp(-r * t) * (sum_payoff / float(N))
                
            let BSput_MC (N:int) (n:float) (S0:float) (K:float) (r:float) (o:float) (t:float) = 
                let L = [for i in 1 .. N -> (listSi n S0 r o t)[Convert.ToInt32(n)-1] ]
                let sum_payoff = List.sumBy (fun elem -> if elem < K then K - elem else 0) L
                exp(-r * t) * (sum_payoff / float(N))

            //BScall_MC N n S0 K r o t    // return expected price of Call option
            //BSput_MC N n S0 K r o t 
        

        module Asian = 
            // lets make k - used once for creating random variables
            let k n :int = Convert.ToInt32(Math.Ceiling(n/2.))
            // consts - constant values in Simulation of Geometric Brownian Motion (to not calculate them over again)
            let consts n r o t = ((r-o*o/2.)*t/n, o*sqrt(t/n))

            // Z - is a list of are two independent standard normal random variables
            let listZpom k = [ for i in 1 .. k -> Random.N1_N2() ]
            let Zi listZpom = List.map (fun (x,y) -> x) listZpom @ List.map (fun (x,y) -> y) listZpom
            let Z = k >> listZpom >> Zi // Random variables used in Geometric Brownian Motion

            // Si - recursive function defined in Geometric Brownian Motion
            let rec Si (i:int) (S0:float) (constOne:float, constTwo:float) (Z:float list) = 
                match i with
                | 0 -> S0 
                | _ -> Si (i-1) (S0) (constOne, constTwo) (Z) * exp(constOne + constTwo * Z[i])

            // S - for convenience, pass just initial arguments
            let S i n S0 r o t =   Si (i) (S0) (consts n r o t) (Z n) 

            // listSi - list of S for one path helpful with calculation of Ri
            let listSi (n:float) S0 r o t  = [for i in 1 .. Convert.ToInt32(n) -> S (i-1) n S0 r o t ]

            let BScall_MC (N:int) (n:float) (S0:float) (K:float) (r:float) (o:float) (t:float) = 
                let L = [for i in 1 .. N -> List.average (listSi n S0 r o t) ]
                let sum_payoff = List.sumBy (fun elem -> if elem > K then elem - K else 0) L
                exp(-r * t) * (sum_payoff / float(N))
                
            let BSput_MC (N:int) (n:float) (S0:float) (K:float) (r:float) (o:float) (t:float) = 
                let L = [for i in 1 .. N -> List.average (listSi n S0 r o t) ]
                let sum_payoff = List.sumBy (fun elem -> if elem < K then K - elem else 0) L
                exp(-r * t) * (sum_payoff / float(N))

            //BScall_MC N n S0 K r o t    // return expected price of Call option
            //BSput_MC N n S0 K r o t 
        

        module American = 
            // lets make k - used once for creating random variables
            let k n :int = Convert.ToInt32(Math.Ceiling(n/2.))
            // consts - constant values in Simulation of Geometric Brownian Motion (to not calculate them over again)
            let consts n r o t = ((r-o*o/2.)*t/n, o*sqrt(t/n))

            // Z - is a list of are two independent standard normal random variables
            let listZpom k = [ for i in 1 .. k -> Random.N1_N2() ]
            let Zi listZpom = List.map (fun (x,y) -> x) listZpom @ List.map (fun (x,y) -> y) listZpom
            let Z = k >> listZpom >> Zi // Random variables used in Geometric Brownian Motion

            // Si - recursive function defined in Geometric Brownian Motion
            let rec Si (i:int) (S0:float) (constOne:float, constTwo:float) (Z:float list) = 
                match i with
                | 0 -> S0 
                | _ -> Si (i-1) (S0) (constOne, constTwo) (Z) * exp(constOne + constTwo * Z[i])

            // S - for convenience, pass just initial arguments
            let S i n S0 r o t =   Si (i) (S0) (consts n r o t) (Z n) 

            // listSi - list of S for one path helpful with calculation of Ri
            let listSi (n:float) S0 r o t  = [for i in 1 .. Convert.ToInt32(n) -> S (i-1) n S0 r o t ]

            let BScall_MC (N:int) (n:float) (S0:float) (K:float) (r:float) (o:float) (t:float) (take_profit:float) = 
                let take_profit_func take_profit elem =  elem >= take_profit 
                let result (take_profit:float) (list:float list) = 
                    match List.tryFind (take_profit_func take_profit) list with
                    | Some value -> value
                    | None -> 0
                let L = [for i in 1 .. N -> result take_profit (listSi n S0 r o t) ]
                let sum_payoff = List.sumBy (fun elem -> if elem > K then elem - K else 0) L
                exp(-r * t) * (sum_payoff / float(N))
                
            let BSput_MC (N:int) (n:float) (S0:float) (K:float) (r:float) (o:float) (t:float) (take_profit:float) = 
                let take_profit_func take_profit elem =  elem <= take_profit 
                let result (take_profit:float) (list:float list) = 
                    match List.tryFind (take_profit_func take_profit) list with
                    | Some value -> value
                    | None -> 0
                let L = [for i in 1 .. N -> result take_profit (listSi n S0 r o t) ]
                let sum_payoff = List.sumBy (fun elem -> if elem < K then K - elem else 0) L
                exp(-r * t) * (sum_payoff / float(N))

            let take_profit_call = 110
            let take_profit_put = 90
            // BScall_MC N=1 n=1. S0 K r o t take_profit_call   // return expected price of Call option
            // BSput_MC N n S0 K r o t take_profit_put
        ;;

(*END OF FILE*)        