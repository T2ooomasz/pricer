module Valuation
open Trades
open Payment
open OptionS

let valuateTrade config marketData (trade : Trade) : Trade =
  match trade with
  | Payment p -> 
      let inputs = 
        { Trade = p
          Data = config
          MarketData = marketData
        }
      let vm = PaymentValuationModel(inputs)
      Payment { p with Value = Some <| vm.Calculate()}

  | OptionS o -> 
      let inputs = 
        { TradeS = o
          DataS = config
          MarketDataS = marketData
        }
      let vm = OptionValuationModel3(inputs)
      let ValueMcHelp = Some <| vm.CalculateMc()
      let ValueAnHelp = Some <| vm.CalculateAn()
      let DeltaHelp   = vm.CalculateDelta()
      OptionS { o with ValueMc = ValueMcHelp; ValueAn = ValueAnHelp; Delta = DeltaHelp}
      



