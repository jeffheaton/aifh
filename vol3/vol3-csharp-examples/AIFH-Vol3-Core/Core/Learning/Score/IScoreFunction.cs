
namespace AIFH_Vol3.Core.Learning.Score
{
    /// <summary>
    /// A score function evaluates a Machine Learning algorithm.  We will seek to either minimize or maximize the score.
    /// </summary>
    public interface IScoreFunction
    {
        /// <summary>
        /// Calculate a score for the specified algorithm.
        /// </summary>
        /// <param name="algo">The algorithm to score.</param>
        /// <returns>The score.</returns>
        double CalculateScore(IMLMethod algo);
    }
}
