using System.Text;

namespace Plastic.Helpers
{
    public static class BMAM
    {
        public static char[] text = { ' ', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
                's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0' };

        public static Dictionary<string, string> map = new Dictionary<string, string>()
        {
            { ".-", "a" },
            { "-...", "b" },
            { "-.-.", "c" },
            { "-..", "d" },
            { ".", "e" },
            { "..-.", "f" },
            { "--.", "g" },
            { "....", "h" },
            { "..", "i" },
            { ".---", "j" },
            { "-.-", "k" },
            { ".-..", "l" },
            { "--", "m" },
            { "-.", "n" },
            { "---", "o" },
            { ".--.", "p" },
            { "--.-", "q" },
            { ".-.", "r" },
            { "...", "s" },
            { "-", "t" },
            { "..-", "u" },
            { "...-", "v" },
            { ".--", "x" },
            { "-..-", "y" },
            { "-.--", "z" },
            { "--..", " " },
        };

        public static string ToBinary(string text)
        {
            StringBuilder binary = new StringBuilder();
            foreach (char c in text)
            {
                binary.Append(Convert.ToString(c, 2).PadLeft(8, '0') + " ");
            }
            return binary.ToString().Trim();
        }
        public static string FromBinary(string binary)
        {
            StringBuilder text = new StringBuilder();
            string[] binaryValues = binary.Split(' ');
            foreach (string binaryValue in binaryValues)
            {
                int value = Convert.ToInt32(binaryValue, 2);
                text.Append(value.ToString());
            }
            return text.ToString();
        }

        public static string ToMorse(string toTranslate)
        {
            var charToMorse = new Dictionary<char, string>
            {
                { 'a', ".-" },    { 'b', "-..." },  { 'c', "-.-." }, { 'd', "-.." },
                { 'e', "." },     { 'f', "..-." },  { 'g', "--." },  { 'h', "...." },
                { 'i', ".." },    { 'j', ".---" },  { 'k', "-.-" },  { 'l', ".-.." },
                { 'm', "--" },    { 'n', "-." },    { 'o', "---" },  { 'p', ".--." },
                { 'q', "--.-" },  { 'r', ".-." },   { 's', "..." },  { 't', "-" },
                { 'u', "..-" },   { 'v', "...-" },  { 'w', ".--" },  { 'x', "-..-" },
                { 'y', "-.--" },  { 'z', "--.." },  { '1', ".----" },{ '2', "..---" },
                { '3', "...--" }, { '4', "....-" }, { '5', "....." },{ '6', "-...." },
                { '7', "--..." }, { '8', "---.." }, { '9', "----." },{ '0', "-----" },
                { ' ', "/" }
            };

            var sb = new StringBuilder();
            foreach (char c in toTranslate.ToLowerInvariant())
            {
                if (charToMorse.TryGetValue(c, out var morse))
                {
                    sb.Append(morse).Append(' ');
                }
            }
            return sb.ToString().TrimEnd();
        }

        public static string DecodeMorse(string morse)
        {
            var morseToChar = new Dictionary<string, char>
            {
                { ".-", 'a' },    { "-...", 'b' },  { "-.-.", 'c' }, { "-..", 'd' },
                { ".", 'e' },     { "..-.", 'f' },  { "--.", 'g' },  { "....", 'h' },
                { "..", 'i' },    { ".---", 'j' },  { "-.-", 'k' },  { ".-..", 'l' },
                { "--", 'm' },    { "-.", 'n' },    { "---", 'o' },  { ".--.", 'p' },
                { "--.-", 'q' },  { ".-.", 'r' },   { "...", 's' },  { "-", 't' },
                { "..-", 'u' },   { "...-", 'v' },  { ".--", 'w' },  { "-..-", 'x' },
                { "-.--", 'y' },  { "--..", 'z' },  { "-----", '0' },{ ".----", '1' },
                { "..---", '2' }, { "...--", '3' }, { "....-", '4' },{ ".....", '5' },
                { "-....", '6' }, { "--...", '7' }, { "---..", '8' },{ "----.", '9' },
                { "/", ' ' }
            };

            var words = morse.Trim().Split(new[] { "   " }, StringSplitOptions.None);
            var decodedWords = words.Select(word =>
            {
                var letters = word.Split(' ');
                var decodedLetters = letters.Select(letter =>
                    morseToChar.TryGetValue(letter, out var ch) ? ch.ToString() : "?");
                return string.Concat(decodedLetters);
            });
            return string.Join(" ", decodedWords);
        }

        public static IEnumerable<string> DecodeMorseINum(string morse)
        {
            var letters =
                map
                    .Where(kvp => morse.StartsWith(kvp.Key))
                    .Select(kvp => new
                    {
                        letter = kvp.Value,
                        remainder = morse.Substring(kvp.Key.Length)
                    })
                    .ToArray();
            if (letters.Any())
            {
                var query =
                    from l in letters
                    from x in DecodeMorse(l.remainder)
                    select l.letter + x;
                return query.ToArray();
            }
            else
            {
                return new[] { "" };
            }
        }

        public static string HashText(string passOrInput)
        {
            byte[] data = SHA256.ComputeHash(Encoding.UTF8.GetBytes(passOrInput));

            StringBuilder sBuilder = new StringBuilder();

            for (int i = 0; i < data.Length; i++)
            {
                sBuilder.Append(data[i].ToString("x2"));
            }

            return sBuilder.ToString();
        }

        public static bool VerifyHash(string input, string hash)
        {
            string hashedInput = HashText(input);
            return string.Equals(hashedInput, hash, StringComparison.OrdinalIgnoreCase);
        }
    }
}
