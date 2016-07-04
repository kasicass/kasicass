namespace Illusion
{
    /// <summary>
    ///  Base transaction class.
    /// </summary>
    public class TransactionBase : ITransaction
    {
        #region ctors

        /// <summary>
        /// Initializes a new instance of the <see cref="TransactionBase"/> class.
        /// </summary>
        /// <param name="am">The am.</param>
        /// <param name="isDelayed">if set to <c>true</c> [is delayed].</param>
        public TransactionBase(ActionManager am, bool isDelayed)
            : this(am)
        {
            IsDelayed = isDelayed;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="TransactionBase"/> class.
        /// </summary>
        /// <param name="am">The am.</param>
        public TransactionBase(ActionManager am)
            : this()
        {
            ActionManager = am;
            if (am != null)
            {
                am.OpenTransaction(this);
            }
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="TransactionBase"/> class.
        /// </summary>
        public TransactionBase()
        {
            IsDelayed = true;
        }

        #endregion

        /// <summary>
        /// Gets or sets the action manager.
        /// </summary>
        /// <value>The action manager.</value>
        public ActionManager ActionManager { get; private set; }
        /// <summary>
        /// Gets or sets a value indicating whether this instance is delayed.
        /// </summary>
        /// <value>
        /// 	<c>true</c> if this instance is delayed; otherwise, <c>false</c>.
        /// </value>
        public bool IsDelayed { get; set; }

        /// <summary>
        ///  accumulating Action
        /// </summary>
        protected IMultiAction accumulatingAction;
        /// <summary>
        /// Gets the accumulating action.
        /// </summary>
        /// <value>The accumulating action.</value>
        public IMultiAction AccumulatingAction
        {
            get
            {
                return accumulatingAction;
            }
        }

        /// <summary>
        /// Commits this instance.
        /// </summary>
        public virtual void Commit()
        {
            if (ActionManager != null)
            {
                ActionManager.CommitTransaction();
            }
        }

        /// <summary>
        /// Rollbacks this instance.
        /// </summary>
        public virtual void Rollback()
        {
            if (ActionManager != null)
            {
                ActionManager.RollBackTransaction();
                Aborted = true;
            }
        }

        /// <summary>
        /// Gets or sets a value indicating whether this <see cref="TransactionBase"/> is aborted.
        /// </summary>
        /// <value><c>true</c> if aborted; otherwise, <c>false</c>.</value>
        public bool Aborted { get; set; }

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        public virtual void Dispose()
        {
            if (!Aborted)
            {
                Commit();
            }
        }
    }
}
