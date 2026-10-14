using Microsoft.AspNetCore.Mvc;

[Route("api/blogs")]
[ApiController]
public class BlogController : ControllerBase
{
  private static List<Blog> blogs = new List<Blog>();

  [HttpGet]
  public ActionResult<List<Blog>> GetAll() => Ok(blogs);

  [HttpGet("{id}")]
  public ActionResult<Blog> GetById(int id)
  {
    var blog = blogs.FirstOrDefault(b => b.Id == id);
    return blog != null ? Ok(blog) : NotFound();
  }

  [HttpPost]
  public ActionResult Create(Blog newBlog)
  {
    newBlog.Id = blogs.Count + 1;
    blogs.Add(newBlog);
    return CreatedAtAction(nameof(GetById), new { id = newBlog.Id }, newBlog);
  }

  [HttpPut("{id}")]
  public ActionResult Update(int id, Blog updatedBlog)
  {
    var blog = blogs.FirstOrDefault(b => b.Id == id);
    if (blog == null) return NotFound();

    blog.Title = updatedBlog.Title;
    blog.Body = updatedBlog.Body;

    return Ok(blog);
  }

  [HttpDelete("{id}")]
  public ActionResult Delete(int id)
  {
    var blog = blogs.FirstOrDefault(b => b.Id == id);
    if (blog == null) return NotFound();

    blogs.Remove(blog);
    return NoContent();
  }
}