using Serilog;

var builder = WebApplication.CreateBuilder(args);

Log.Logger = new LoggerConfiguration()
  .WriteTo.Console()
  .WriteTo.File("logs/myapp.txt", rollingInterval: RollingInterval.Day)
  .CreateLogger();
builder.Host.UseSerilog();

builder.Services.AddControllers();
builder.Services.AddSingleton<IMyService, MyService>();

var app = builder.Build();

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