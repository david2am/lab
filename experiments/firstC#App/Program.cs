var builder = WebApplication.CreateBuilder(args);

builder.Services.AddControllers();
builder.Services.AddHttpLogging();

var app = builder.Build();

app.UseHttpLogging();

app.MapControllers();

app.MapGet("/", () => "Hello world");

app.Run();