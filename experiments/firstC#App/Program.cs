var builder = WebApplication.CreateBuilder(args);
builder.Services.AddControllers();

var app = builder.Build();

app.Use(async (context, next) =>
{
  await next.Invoke();
});

app.MapControllers();

app.Run();