module Trades
open Payment
open OptionS

type Trade = 
    |   Payment of PaymentRecord
    |   OptionS of OptionRecord

type TradeID = System.Guid

let newTradeID () : TradeID= System.Guid.NewGuid()
