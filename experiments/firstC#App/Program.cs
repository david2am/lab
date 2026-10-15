using System.Text.Json;
using System.Security.Cryptography;
using System.Text;

public class Program
{
  public class User
  {
    public string Name { get; set; }
    public string Email { get; set; }
    public string Password { get; set; }

    public string GenerateHash()
    {
      using (SHA256 sha256 = SHA256.Create())
      {
        byte[] hashBytes = sha256.ComputeHash(Encoding.UTF8.GetBytes(ToString()));
        return Convert.ToBase64String(hashBytes);
      }
    }

    public void EncryptData()
    {
      Password = Convert.ToBase64String(Encoding.UTF8.GetBytes(Password));
    }
  }

  public static User DeserializeUserData(string jsonData, bool isTrustedSource)
    {
      if(!isTrustedSource)
      {
        Console.WriteLine("Deserialization blocked: Untrusted source.");
        return null;
      }

      return JsonSerializer.Deserialize<User>(jsonData);
    }

    public static string SerializeUserData(User user)
    {
      if (string.IsNullOrWhiteSpace(user.Name)
        || string.IsNullOrWhiteSpace(user.Email)
        || string.IsNullOrWhiteSpace(user.Password)
      )
      {
        Console.WriteLine("Invalid data. Serialization aborted.");
        return string. Empty;
      }

      user.EncryptData();
      return JsonSerializer.Serialize(user);
    }

  public static void Main()
  {
    User user = new User
    {
      Name = "Alice",
      Email = "alice@email.com",
      Password = "someStrongPWD123"
    };

    string generateHash = user.GenerateHash();
    string serializedData = SerializeUserData(user);
    User deserializeData = DeserializeUserData(serializedData, true);
    Console.WriteLine($"Serialized Data:\n {generateHash}");
  }
}