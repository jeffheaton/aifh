using System;

namespace AIFH_Vol2
{
    public class ExampleHolder
    {
        public string ExampleName { get; set; }
        public int ExampleChapter { get; set; }
        public Type ExampleType { get; set; }

        public ExampleHolder(int chapterNumber, string exampleName, Type type)
        {
            ExampleName = exampleName;
            ExampleChapter = chapterNumber;
            ExampleType = type;
        }
    }
}
