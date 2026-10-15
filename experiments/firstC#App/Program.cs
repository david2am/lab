using System.Text.Json;
using System.Xml.Serialization;
using Serilog;

var builder = WebApplication.CreateBuilder(args);

Log.Logger = new LoggerConfiguration()
  .WriteTo.Console()
  .WriteTo.File("logs/myapp.txt", rollingInterval: RollingInterval.Day)
  .CreateLogger();
builder.Host.UseSerilog();

builder.Services.AddControllers();
builder.Services.AddSingleton<IMyService, MyService>();
builder.Services.ConfigureHttpJsonOptions(options =>
{
  options.SerializerOptions.PropertyNamingPolicy = JsonNamingPolicy.KebabCaseUpper;
});

var app = builder.Build();

var samplePerson = new Person { UserName = "Alice", UserAge = 30 };

app.Use(async (context, next) =>
{
  try
  {
    await next();
  }
  catch (Exception ex)
  {
    Console.WriteLine($"Global exception caught: {ex}");
    context.Response.StatusCode = 500;
    await context.Response.WriteAsync("An unexpected error occured. Please try again later.");
  }
});

app.Use(async (context, next) =>
{
  var myService = context.RequestServices.GetRequiredService<IMyService>();
  myService.LogCreation("First Middleware");
  await next.Invoke();
});

app.MapControllers();

app.MapGet("/", () =>
{
  // var jsonString = JsonSerializer.Serialize(samplePerson);
  // return TypedResults.Text(jsonString, "application/json");
  // var options = new JsonSerializerOptions

  // {
  //   PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower
  // };
  // var customJsonString = JsonSerializer.Serialize(samplePerson, options);
  // return TypedResults.Text(customJsonString, "application/json");
  
//  return TypedResults.Json(samplePerson);

  return samplePerson;

  // var xmlSerializer = new XmlSerializer(typeof (Person)); 
  // var strinWriter = new StringWriter();
  // xmlSerializer.Serialize(strinWriter, samplePerson);
  // var xmlOutput = strinWriter.ToString();

  // return TypedResults.Text(xmlOutput, "application/xml");
});

app.Run();


public interface IMyService
{
  void LogCreation(string message);
}

public class MyService: IMyService
{
  private readonly int _serviceId;

  public MyService()
  {
    _serviceId = new Random().Next(100000, 999999);
  }

  public void LogCreation(string message)
  {
    Console.WriteLine($"{message} - Service ID: {_serviceId}");
  }
}

public class Person
{
  required public string UserName { get; set; }
  required public int UserAge { get; set; }
}