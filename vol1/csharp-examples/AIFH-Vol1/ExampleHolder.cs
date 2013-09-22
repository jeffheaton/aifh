using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol1
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
