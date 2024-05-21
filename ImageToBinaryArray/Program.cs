using System;
using System.IO;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;
using static System.Console;

namespace ImageToBinaryArray
{
    class Program
    {
        [STAThread]
        static void Main(string[] args)
        {
            WriteLine("Array(c) or Atari(a) bytes?");
            if(ReadKey().KeyChar == 'c')
            {
                while (true)
                {
                    WriteLine();
                    Write("Path: ");
                    string path = ReadLine();
                    Image<Rgba32> loadedImage = (Image<Rgba32>)Image.Load(path.Replace("\"", ""));
                    Span<Rgba32> rgbSpan;
                    for (int y = 0; y < loadedImage.Height; y++)
                    {
                        rgbSpan = loadedImage.GetPixelRowSpan(y);
                        if (loadedImage.Width > 8)
                            Write("{");
                        for (int x = 0; x < loadedImage.Width; x++)
                        {
                            if (x % 8 == 0)
                                Write("B");

                            if (rgbSpan[x].R > 0)
                                Write("1");
                            else
                                Write("0");
                            if (x % 8 == 7 && loadedImage.Width - x > 1)
                                Write(",");
                        }
                        if (loadedImage.Width > 8)
                            Write("}");
                        WriteLine(",");
                    }
                    ReadKey();
                }
            }
            else
            {
                bool flipSpriteY = false, flipSpriteX = false;
                while (true)
                {
                    WriteLine();
                    Write("Path: ");
                    string path = ReadLine();
                    Image<Rgba32> loadedImage = (Image<Rgba32>)Image.Load(path.Replace("\"", ""));
                    Span<Rgba32> rgbSpan;
                    WriteLine("Flip Vertically? y/n");
                    if (ReadKey().KeyChar == 'y')
                        flipSpriteY = true;
                    else
                        flipSpriteY = false;
                    WriteLine();
                    WriteLine("Flip Horizontally? y/n");
                    if (ReadKey().KeyChar == 'y')
                        flipSpriteX = true;
                    else
                        flipSpriteX = false;
                    WriteLine();

                    for (int y = 0; y < loadedImage.Height; y++)
                    {
                        if(flipSpriteY)
                            rgbSpan = loadedImage.GetPixelRowSpan(loadedImage.Height - 1 - y);
                        else
                            rgbSpan = loadedImage.GetPixelRowSpan(y);
                        for (int x = 0; x < Math.Min(loadedImage.Width, 8); x++)
                        {
                            if (x == 0)
                                Write("\t.byte #%");

                            if (rgbSpan[flipSpriteX ? Math.Min(loadedImage.Width, 8) - 1 - x : x].R > 0)
                                Write("1");
                            else
                                Write("0");
                        }
                        WriteLine("");
                    }
                }
            }
        }
    }
}
