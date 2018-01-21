#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\SQLProvider\lib\net451\FSharp.Data.SqlProvider.dll"
#endif
namespace fSharpExperiments

module MySql = 
    open FSharp.Data.Sql

    [<Literal>]
    let connString  = "Server=pi3debian1;Database=northwind;User=jack;Password=kJyoZYyRB0Iv0JmfZ7tq"
    [<Literal>]
    let dbVendor    = Common.DatabaseProviderTypes.MYSQL
    [<Literal>]
    let resPath = @"C:\Users\jackc\Documents\Git\fsharpExperiments\packages\MySql.Data\lib\net452"
    [<Literal>]
    let indivAmount = 1000
    [<Literal>]
    let useOptTypes = true

    type sql = SqlDataProvider<
                    dbVendor,
                    connString,
                    ResolutionPath = resPath,
                    IndividualsAmount = indivAmount,
                    UseOptionTypes = useOptTypes
                >
    let ctx = sql.GetDataContext()

    [<Literal>] 
    let resPathMySqlConnector = @"C:\Users\jackc\Documents\Git\fsharpExperiments\packages\MySqlConnector\lib\net46"

    type sqlNew = SqlDataProvider<Common.DatabaseProviderTypes.MYSQL, 
                                    connString, 
                                    ResolutionPath = resPathMySqlConnector >
    let ctxNew = sqlNew.GetDataContext()

    let employees = 
        ctx.Northwind.Employees 
        |> Seq.map (fun e -> e.ColumnValues |> Seq.toList)
        |> Seq.toList

    let e2 = 
        ctxNew.Northwind.Customers
        |> Seq.map (fun e -> e.ColumnValues |> Seq.toList)
        |> Seq.toList

    let KarenOrderDetails =
        query { for c in ctxNew.Northwind.Customers do
                // you can directly enumerate relationships with no join information
                for o in c.``northwind.orders by customer_id`` do
                // or you can explicitly join on the fields you choose
                join od in ctxNew.Northwind.OrderDetails on (o.Id = od.OrderId)
                //  the (!!) operator will perform an outer join on a relationship
                for prod in (!!) od.``northwind.products by product_id`` do 
                // nullable columns can be represented as option types; the following generates IS NOT NULL
                // standard operators will work as expected; the following shows the like operator and IN operator
                where (c.FirstName =% ("Karen%") && c.Company |=| [|"Company AA";"DaveCompant"|] )
                sortBy o.ShipName
                // arbitrarily complex projections are supported
                select (c.FirstName,o.ShipAddress,o.ShipCountryRegion,prod.ProductName,prod.ListPrice) } 
        |> Seq.toArray

    type sqlClassicModels = SqlDataProvider<Common.DatabaseProviderTypes.MYSQL, 
                                                "Server=pi3debian1;User=jack;Database=classicmodels;Password=kJyoZYyRB0Iv0JmfZ7tq", 
                                                ResolutionPath = resPathMySqlConnector >
    let ctxClassicModels = sqlClassicModels.GetDataContext()

    FSharp.Data.Sql.Common.QueryEvents.SqlQueryEvent |> Event.add (printfn "Executing SQL: %O")


    let employeesFirstName = 
        query {
            for emp in ctxClassicModels.Classicmodels.Employees do
            select (emp.FirstName)
            } |> Seq.head

    let customersQuery = 
        query { 
            for customer in ctxClassicModels.Classicmodels.Customers do
                select customer
        }
        |> Seq.length
    
    ()


module MySqlds18b20 = 
    open FSharp.Data.Sql

    [<Literal>]
    let connString  = "Server=pi3debian1;Database=turd;User=jack;Password=kJyoZYyRB0Iv0JmfZ7tq"
    [<Literal>]
    let dbVendor    = Common.DatabaseProviderTypes.MYSQL
    [<Literal>]
    let resPath = @"C:\Users\jackc\Documents\Git\fsharpExperiments\packages\MySql.Data\lib\net452"
    [<Literal>]
    let indivAmount = 1000
    [<Literal>]
    let useOptTypes = true

    type sql = SqlDataProvider<
                    dbVendor,
                    connString,
                    ResolutionPath = resPath,
                    IndividualsAmount = indivAmount,
                    UseOptionTypes = useOptTypes
                >
    let ctx = sql.GetDataContext()
    
    ctx.Turd.Ds18b20.``Create(datetime, sensor, temperature)``(System.DateTime.Now,"041684B58AFF",(float32 19.375)) |> ignore
    ctx.Turd.Ds18b20.``Create(datetime, sensor, temperature)``(System.DateTime.Now,"041684A9F1FF",(float32 18.625)) |> ignore
    ctx.Turd.Ds18b20.``Create(datetime, sensor, temperature)``(System.DateTime.Now,"041684AC4BFF",(float32 18.8125)) |> ignore
    ctx.Turd.Ds18b20.``Create(datetime, sensor, temperature)``(System.DateTime.Now,"0416848A0FFF",(float32 21.0625)) |> ignore
    ctx.SubmitUpdates()

    let tempQuery = 
        query { 
            for record in ctx.Turd.Ds18b20 do
                select record
        }

    let first = tempQuery |> Seq.head
