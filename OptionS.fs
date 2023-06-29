module OptionS
open System
open Configuration
open Money
open Utils
open Calculation


(* Model for Option trade. *)
type OptionRecord =
    {
        OptionID      : string
        Stock         : string
        OptionType    : string
        Expiry        : DateTime
        ExpectedPrice : float32
        Drift         : float32
        Volatility    : float32
        Principal     : int64
        Currency      : string
        ValueMc       : Money option
        ValueAn       : Money option
        Delta         : float32
    }
    
    (* Simple utility method for creating a random Option. *)
    static member sysRandom = System.Random()

    static member Random(marketData : MarketData) = 
        (* We pick a random currency either from given short list, or from valuation::knownStocks config key *)
        let knownStocksDefault = [| "AAPL"; "GOOGL"; "MSFT"; "AMZN";|]
        
        let knownStocks = if marketData.ContainsKey "valuation::knownStocks" 
                              then marketData.["valuation::knownStocks"].Split([|' '|])
                              else knownStocksDefault

        let knownOptionTypeDefault = [| "EurCall"; "EurPut"; |]
        
        let knownOptionType = if marketData.ContainsKey "valuation::knownOptionType" 
                              then marketData.["valuation::knownOptionType"].Split([|' '|])
                              else knownOptionTypeDefault
        
        {
            OptionID = sprintf "ID::%04d" (OptionRecord.sysRandom.Next(9999))
            Stock = sprintf "%s" (knownStocks.[ OptionRecord.sysRandom.Next(knownStocks.Length) ])
            OptionType = sprintf "%s" (knownOptionType.[ OptionRecord.sysRandom.Next(knownOptionType.Length) ])
            Expiry    = (DateTime.Now.AddMonths (OptionRecord.sysRandom.Next(1, 6))).Date
            ExpectedPrice = float32 (100)
            Drift = float32 0.1
            Volatility = float32 0.1
            Principal = int64 10
            Currency  = "USD"
            ValueMc = None
            ValueAn = None
            Delta = float32 0.0
        }

(* Complete set of data required for valuation *)
type OptionValuationInputs = 
    {
        TradeS : OptionRecord
        DataS : Configuration
        MarketDataS: MarketData
    }

(* The valuation model for Option. We may have multiple valuation models implementations per given trade type, or have a valuation model that handles multiple trade types. *)
type OptionValuationModel3(inputs: OptionValuationInputs) = 
    (* Calculate() method returns a value of given trade. This one is very simple, yet demonstrates some concepts.
    
    It will try to return the result in the global default currency as configured by valuation::baseCurrency key.

    If the valuation::baseCurrency is not defined or we are unable to obtain the FX rate FX::<targetCcy><tradeCcy>, 
    we simply return the value using the trade currency.

    *)
    member this.CalculateMc() : Money = 
        let tradeCcy = inputs.TradeS.Currency

        let targetCcy = match inputs.MarketDataS.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.DataS.ContainsKey fxRateKey then 1. /  float inputs.DataS.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.DataS.ContainsKey fxRateKey then targetCcy else tradeCcy

        let runsStr = match inputs.MarketDataS.TryFind "monteCarlo:runs" with
                         | Some x -> x
                         | None -> "100"

        let runs = int (match (Int64.TryParse(runsStr) |> Utils.ofBool) with
                         | Some x -> x 
                         | None -> 100)

        let stepsStr = match inputs.MarketDataS.TryFind "monteCarlo:steps" with
                         | Some x -> x
                         | None -> "100"
                         
        let steps = int (match (Int64.TryParse(stepsStr) |> Utils.ofBool) with
                         | Some x -> x 
                         | None -> 50)

        let tradeStock = inputs.TradeS.Stock

        

        let priceStockKey = sprintf "NYSE::%s" tradeStock

        let S0 = if inputs.DataS.ContainsKey priceStockKey then float inputs.DataS.[ priceStockKey ] else -0.0 // lookup stock price
        let K = (float inputs.TradeS.ExpectedPrice)
        let r = (float inputs.TradeS.Drift)
        let o = (float inputs.TradeS.Volatility)
        let Td = (inputs.TradeS.Expiry - DateTime.Now)
        let T = (float Td.Days) / 365.
        
        let takeProfitStr = match inputs.MarketDataS.TryFind "valuation::takeprofit" with
                            | Some x -> x
                            | None -> "10"

        let takeProfitHelp = float (match (Single.TryParse(takeProfitStr) |> Utils.ofBool) with
                                    | Some x -> x 
                                    | None -> (float32 10.))
        

        let f optionType = match optionType with
                            | "EurCall" ->  Calculation.MonteCarlo.European.BScall_MC runs steps S0 K r o T
                            | "EurPut"  ->  Calculation.MonteCarlo.European.BSput_MC runs steps S0 K r o T
                            | "AsiCall" ->  Calculation.MonteCarlo.Asian.BScall_MC runs steps S0 K r o T
                            | "AsiPut"  ->  Calculation.MonteCarlo.Asian.BSput_MC runs steps S0 K r o T
                            | "AmCall"  ->  Calculation.MonteCarlo.American.BScall_MC runs steps S0 K r o T ((takeProfitHelp + 100.) * S0)
                            | "AmPut"   ->  Calculation.MonteCarlo.American.BSput_MC runs steps S0 K r o T ((-takeProfitHelp + 100.) * S0)
                            | _ -> -0.

        let optionType = inputs.TradeS.OptionType
        let valueMc = (f optionType) * float (inputs.TradeS.Principal)

        { Value = valueMc; Currency = finalCcy; }

    member this.CalculateAn() : Money = 
        let tradeCcy = inputs.TradeS.Currency

        let targetCcy = match inputs.MarketDataS.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.DataS.ContainsKey fxRateKey then 1. /  float inputs.DataS.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.DataS.ContainsKey fxRateKey then targetCcy else tradeCcy

        let tradeStock = inputs.TradeS.Stock

        let priceStockKey = sprintf "NYSE::%s" tradeStock

        let S0 = if inputs.DataS.ContainsKey priceStockKey then float inputs.DataS.[ priceStockKey ] else -0.0 // lookup stock price
        let K = (float inputs.TradeS.ExpectedPrice)
        let r = (float inputs.TradeS.Drift)
        let o = (float inputs.TradeS.Volatility)
        let Td = (inputs.TradeS.Expiry - DateTime.Now)
        let T = (float Td.Days) / 365.
        
        
        let f optionType = match optionType with
                            | "EurCall" ->  Calculation.Analytical.BScall Calculation.Analytical.phiFunc Calculation.Analytical.d1 S0 K r T o 
                            | "EurPut"  ->  Calculation.Analytical.BSput Calculation.Analytical.phiFunc Calculation.Analytical.d1 S0 K r T o 
                            | "AsiCall" ->  -0.
                            | "AsiPut"  ->  -0.
                            | "AmCall"  ->  -0.
                            | "AmPut"   ->  -0.
                            | _ -> -0.

        let optionType = inputs.TradeS.OptionType
        let valueAn = (f optionType) * float (inputs.TradeS.Principal)

        { Value = valueAn; Currency = finalCcy; }

    member this.CalculateDelta() : float32 = 
        let tradeCcy = inputs.TradeS.Currency

        let targetCcy = match inputs.MarketDataS.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.DataS.ContainsKey fxRateKey then 1. /  float inputs.DataS.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.DataS.ContainsKey fxRateKey then targetCcy else tradeCcy

        let tradeStock = inputs.TradeS.Stock

        let priceStockKey = sprintf "NYSE::%s" tradeStock

        let S0 = if inputs.DataS.ContainsKey priceStockKey then float inputs.DataS.[ priceStockKey ] else -0.0 // lookup stock price
        let K = (float inputs.TradeS.ExpectedPrice)
        let r = (float inputs.TradeS.Drift)
        let o = (float inputs.TradeS.Volatility)
        let Td = (inputs.TradeS.Expiry - DateTime.Now)
        let T = (float Td.Days) / 365.
        
        
        let f optionType = match optionType with
                            | "EurCall" ->  Calculation.Analytical.delta_call S0 K r T o 
                            | "EurPut"  ->  Calculation.Analytical.delta_put S0 K r T o 
                            | "AsiCall" ->  -10.
                            | "AsiPut"  ->  -10.
                            | "AmCall"  ->  -10.
                            | "AmPut"   ->  -10.
                            | _ -> -10.

        let optionType = inputs.TradeS.OptionType
        let DeltaHelp = f optionType

        float32 DeltaHelp; 