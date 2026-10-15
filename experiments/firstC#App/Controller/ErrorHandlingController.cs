using Microsoft.AspNetCore.Mvc;

[Route("api/[controller]")]
[ApiController]
public class ErrorHandlingController : ControllerBase
{
  [HttpGet("division")]
  public IActionResult GetDivisionResult(int num, int den)
  {
    try
    {
      var result = num / den;
      return Ok($"Here's the result: {result}");
    }
    catch (DivideByZeroException)
    {
      Console.WriteLine("Error: Division by zero is not allowed.");
      return BadRequest("Cannot divide by zero.");
    }
  }
}