namespace Illusion
{
    /// <summary>
    ///  Transaction class that support transaction to hold multi actions.
    /// </summary>
    public class Transaction : TransactionBase
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="Transaction"/> class.
        /// </summary>
        /// <param name="actionManager">The action manager.</param>
        /// <param name="delayed">if set to <c>true</c> [delayed].</param>
        protected Transaction(ActionManager actionManager, bool delayed)
            : base(actionManager, delayed)
        {
            this.accumulatingAction = new MultiAction();
        }

        /// <summary>
        /// Creates the specified action manager.
        /// </summary>
        /// <param name="actionManager">The action manager.</param>
        /// <param name="delayed">if set to <c>true</c> [delayed].</param>
        /// <returns></returns>
        public static Transaction Create(ActionManager actionManager, bool delayed)
        {
            return new Transaction(actionManager, delayed);
        }

        /// <summary>
        /// By default, the actions are delayed and executed only after
        /// the top-level transaction commits.
        /// </summary>
        /// <remarks>
        /// Make sure to dispose of the transaction once you're done - it will actually call Commit for you
        /// </remarks>
        public static Transaction Create(ActionManager actionManager)
        {
            return Create(actionManager, true);
        }

        /// <summary>
        /// Commits this instance.
        /// </summary>
        public override void Commit()
        {
            this.AccumulatingAction.IsDelayed = this.IsDelayed;
            base.Commit();
        }
    }
}
