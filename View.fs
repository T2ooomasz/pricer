module View

open Bolero
open Bolero.Html
open Chart
open Messages
open Model
open Money
open Payment
open OptionS
open Radzen.Blazor
open System.Collections.Generic
open Trades

type Templates = Template<"wwwroot/templates.html">
type Main = Template<"wwwroot/main.html">

let keyValueMapDisplay msg name (model: Map<string,string>) dispatch =
    let configRow (kvp : KeyValuePair<string,string>) =
        Templates.ConfigRow()
            .Key(kvp.Key)
            .Value(kvp.Value,fun v -> dispatch <| msg (kvp.Key,v))
            .Elt()
    Templates.KeyValueMapDisplay()
        .Title(text name)
        .Rows(forEach model configRow)
        .Elt()

let configDisplay = keyValueMapDisplay ConfigChange "Configuration"

let tomekDisplay = keyValueMapDisplay ConfigChange "Tomek"
  

let marketDataDisplay = keyValueMapDisplay MarketDataChange "Market Data"
        
let plotLineChart (data : ChartData) =

    let mkSeriesComponent series =
        comp<RadzenLineSeries<ChartItem>>{
          "Smooth" => series.Smooth;
          "CategoryProperty"=>"XValue";
          "Title"=>series.SeriesName;
          "LineType"=> series.Line
          "ValueProperty"=>"YValue"
          "Data" => series.Values;
          attr.fragment "ChildContent"
            <| concat {
              comp<RadzenSeriesDataLabels>{"Visible" => series.ShowLabels}
              comp<RadzenMarkers>{"MarkerType" => series.Marker}
            }
        }

    let Xaxis = comp<RadzenCategoryAxis>{ "Padding"=>20.0; }
    let Yaxis =
        let title = comp<RadzenAxisTitle> { "Text" => data.Title}
        comp<RadzenValueAxis>{ attr.fragment "ChildContent" title }
    let childContent = 
          concat {
              for series in data.Series do
                mkSeriesComponent series
              Xaxis
              Yaxis
          }
    comp<RadzenChart> { attr.fragment "ChildContent" childContent }

let summary (model: Model) dispatch =
    let groupedByCCy =
        model.trades
        |> Map.values
        |> Seq.choose (fun x ->
            match x.trade with
            | Payment p -> p.Value
            | OptionS o -> o.ValueMc
            )
        |> Seq.groupBy (fun m -> m.Currency)
    let summaryRow (ccy,values : Money seq) =
        let sum = values |> Seq.sumBy (fun v -> v.Value)
        Templates.SummaryRow()
            .CCY(text ccy)
            .Value(text <| sprintf "%.2f" sum)
            .Elt()
    Templates.Summary()
        .Rows(forEach groupedByCCy summaryRow)
        .Elt()

let paymentRow dispatch (tradeId, p : PaymentRecord) =
    let value = p.Value |> Option.map (string) |> Option.defaultValue "" 
    let tradeChange msg s = dispatch <| TradeChange (msg (tradeId,s))
    Templates.PaymentsRow()
        .Name(p.TradeName,tradeChange NewName)
        .Expiry(sprintf "%A" p.Expiry, tradeChange NewExpiry)
        .Currency(p.Currency, tradeChange NewCurrency)
        .Principal(sprintf "%i" p.Principal, tradeChange NewPrincipal)
        .Value(value)
        .Delete(fun e -> dispatch (RemoveTrade tradeId))
        .Elt()

let optionRow dispatch (tradeId, p : OptionRecord) =
    let valueMc = p.ValueMc |> Option.map (string) |> Option.defaultValue "" 
    let valueAn = p.ValueAn |> Option.map (string) |> Option.defaultValue "" 
    let tradeChange msg s = dispatch <| TradeChange (msg (tradeId,s))
    Templates.OptionsRow()
        .ID(p.OptionID, tradeChange NewID)
        .Stock(p.Stock, tradeChange NewStock)
        .Type(p.OptionType,tradeChange NewType)
        .Expiry(sprintf "%A" p.Expiry, tradeChange NewExpiry)
        .ExpectedPrice(sprintf "%f" p.ExpectedPrice, tradeChange NewExpectedPrice)
        .Drift(sprintf "%f" p.Drift, tradeChange NewDrift)
        .Principal(sprintf "%i" p.Principal, tradeChange NewPrincipal)
        .Volatility(sprintf "%f" p.Volatility, tradeChange NewVolatility)
        .Currency(p.Currency, tradeChange NewCurrency)
        .ValueMc(valueMc)
        .ValueAn(valueAn)
        .Delta(sprintf "%f" p.Delta, tradeChange NewDelta)
        .Delete(fun e -> dispatch (RemoveTrade tradeId))
        .Elt()

let homePage (model: Model) dispatch =

    let payments = onlyPayments model.trades
    let options = onlyOptions model.trades
    let trades = 
        Templates.Trades()
            .AddPayment(fun _ -> dispatch AddPayment)
            .AddOption(fun _ -> dispatch AddOption)
            .RecalculateAll(fun _ -> dispatch RecalculateAll)
            .PaymentRows(forEach payments (paymentRow dispatch))
            .OptionRows(forEach options (optionRow dispatch))
            .Elt()

    Templates.Home()
     .SummaryPlaceholder(summary model dispatch)
     .TradesPlaceholder(trades)
     .MarketDataPlaceholder(marketDataDisplay model.marketData dispatch)
     .ChartsPlaceholder(plotLineChart model.chart)
     .Elt()

let menuItem (model: Model) (router :Router<_,_,_>) (page: Page) (text: string) =
    let activeFlag = "rz-button rz-secondary"
    Main.MenuItem()
        .Active(if model.page = page then activeFlag else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view router model dispatch =
    Main()
        .Menu(concat {
            menuItem model router Home "Home"
            menuItem model router Config "Config"
            menuItem model router Tomek "Tomek"
        })
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
            | Config -> configDisplay model.configuration dispatch
            | Tomek -> tomekDisplay model.configuration dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty()
            | Some err ->
                Templates.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()