using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;

namespace Illusion
{
	/// <summary>
	/// Denotes the service to handle the <see cref="IOptionPage"/>.
	/// </summary>
	public interface IOptionService
	{
		void Cancel();
		void Commit();

		IOptionPage ActivePage { get; set; }
	}

	/// <summary>
	/// Default imelementation of <see cref="IOptionService"/>.
	/// </summary>
	public class OptionService : IOptionService
	{
		private IOptionPage activePage;

		[ImportMany]
		public IOptionPage[] Pages { get; set; }

		#region IOptionService Members

		public void Cancel()
		{
			Pages.Apply(page => page.Cancel());
		}

		public void Commit()
		{
			Pages.Apply(page => page.Commit());
		}

		public IOptionPage ActivePage
		{
			get
			{
				if (this.activePage != null)
				{
					return this.activePage;
				}
				if (this.Pages.Length > 0)
				{
					return this.Pages[0];
				}
				return null;
			}
			set
			{
				this.activePage = value;
			}
		}

		#endregion
	}
}
