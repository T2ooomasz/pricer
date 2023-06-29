module Update

open Configuration
open Elmish
open Messages
open Model
open Payment
open OptionS
open System
open System.Net.Http
open System.Net.Http.Json
open Trades
open Valuation

let changeTrade (trades : Map<TradeID,UITrade>) id f =
        match Map.tryFind id trades with
        | Some t -> 
            match f t with
            | Some t' -> Map.add id t' trades, Cmd.none
            | None -> trades, Cmd.ofMsg <| Warning (sprintf "could not update trade %s (%A)" t.Name id)
        | None -> trades, Cmd.ofMsg <| Warning (sprintf "could not find trade %A" id)

let tradeChangeUpdate (model : Model) = function
    | NewName (id,name) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> Some <| Payment { p with TradeName = name}
                                | _ -> None
                            )
            )
    | NewPrincipal (id,principal) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> 
                                    Int64.TryParse(principal)
                                    |> Utils.ofBool
                                    |> Option.map (fun principal ->
                                            Payment { p with Principal = principal})
                                | OptionS p -> 
                                    Int64.TryParse(principal)
                                    |> Utils.ofBool
                                    |> Option.map (fun principal ->
                                            OptionS { p with Principal = principal})
                            )
            )

    | NewExpiry (id,expiry) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> 
                                    DateTime.TryParse(expiry)
                                    |> Utils.ofBool
                                    |> Option.map (fun expiry ->
                                            Payment { p with Expiry = expiry})

                                | OptionS p -> 
                                    DateTime.TryParse(expiry)
                                    |> Utils.ofBool
                                    |> Option.map (fun expiry ->
                                           OptionS { p with Expiry = expiry})
                            )
            )

    | NewCurrency (id,ccy) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> Some <| Payment { p with Currency = ccy}
                                | OptionS p -> Some <| OptionS { p with Currency = ccy}
                                ))

    | NewID (id,ids) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | OptionS p -> Some <| OptionS { p with OptionID = ids}
                                | _ -> None
                                ))

    | NewStock (id,stock) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | OptionS p -> Some <| OptionS { p with Stock = stock}
                                 | _ -> None
                                ))

    | NewType (id,typeS) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | OptionS p -> Some <| OptionS { p with OptionType = typeS}
                                 | _ -> None
                                ))

    | NewExpectedPrice (id,exppr) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | OptionS p -> 
                                    Single.TryParse(exppr)
                                    |> Utils.ofBool
                                    |> Option.map (fun exppr ->
                                            OptionS { p with ExpectedPrice = exppr})
                                 | _ -> None
                            )
                )

    | NewDrift (id,r) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | OptionS p -> 
                                    Single.TryParse(r)
                                    |> Utils.ofBool
                                    |> Option.map (fun r ->
                                            OptionS { p with Drift = r})
                                 | _ -> None
                            )
                )

    | NewVolatility (id,o) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | OptionS p -> 
                                    Single.TryParse(o)
                                    |> Utils.ofBool
                                    |> Option.map (fun o ->
                                            OptionS { p with Volatility = o})
                                 | _ -> None
                            )
                )

    | NewDelta (id,D) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | OptionS p -> 
                                    Single.TryParse(D)
                                    |> Utils.ofBool
                                    |> Option.map (fun D ->
                                            OptionS { p with Delta =D})
                                 | _ -> None
                            )
                )

let update (http: HttpClient) message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | AddPayment ->
        let newPayment = Trades.wrap (Payment <| PaymentRecord.Random(model.configuration))
        let newTrades = Map.add newPayment.id newPayment model.trades
        { model with trades = newTrades }, Cmd.none

    | AddOption ->      // HERE OPTIONS FUNCTIONALITY  
        let newOption = Trades.wrap (OptionS <| OptionRecord.Random(model.configuration))
        let newTrades = Map.add newOption.id newOption model.trades
        {model with trades = newTrades}, Cmd.none   

    | RemoveTrade(tradeId) ->
        let newTrades = Map.remove tradeId model.trades
        { model with trades = newTrades }, Cmd.none
    | TradeChange msg ->
        let newTrades,cmd = tradeChangeUpdate model msg
        { model with trades = newTrades }, Cmd.batch [cmd; Cmd.ofMsg RecalculateAll] 
    | ConfigChange (key,value) ->
        let config = model.configuration
        let config' = Map.add key value config
        { model with configuration = config'}, Cmd.none
    | MarketDataChange (key,value) ->
        let md = model.marketData
        let md' = Map.add key value md
        { model with marketData = md'}, Cmd.ofMsg RecalculateAll
    | GotMarketData response ->
        let c = response |> 
                Array.collect (fun cat -> 
                        cat.Config 
                        |> Array.map (fun {Key = k; Value = v} ->
                            sprintf "%s::%s" cat.Category k, v))
                |> Map.ofArray
        { model with marketData = c }, Cmd.none
    | LoadData ->
        let getConfig() = http.GetFromJsonAsync<JsonConfig>("/configuration.json")
        let conf = Cmd.OfTask.either getConfig () GotConfig Error
        let getMDConfig() = http.GetFromJsonAsync<JsonConfig>("/marketDataConfig.json")
        let mdConf = Cmd.OfTask.either getMDConfig () GotMarketData Error
        { model with configuration = Map.empty }, Cmd.batch [conf; mdConf]
    | GotConfig response -> 
        let c = response |> 
                Array.collect (fun cat -> 
                        cat.Config 
                        |> Array.map (fun {Key = k; Value = v} ->
                            sprintf "%s::%s" cat.Category k, v))
                |> Map.ofArray
        { model with configuration = c }, Cmd.none
    | RecalculateAll ->
        let trades =
             model.trades
             |> Map.map (fun _ -> Trades.map <| valuateTrade model.marketData model.configuration)
        { model with trades = trades }, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | Warning err ->
        { model with error = Some err }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
