using System;
using System.ComponentModel.Composition;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Illusion
{

	public interface IPresentation : IHaveName
	{
		void Attach();
		void Detach();
	}

	public interface IPresentationService
	{
		IEnumerable<IPresentation> Presentations { get; }
		IPresentation CurrentPresentation { get; }
		void ChangePresentation(IPresentation presentation);
		void ChangePresentation(string presentationName);
	}

	public class PresentationService : IPresentationService
	{
		#region IPresentationService Members

		[ImportMany]
		public IEnumerable<IPresentation> Presentations { get; set; }

		private IPresentation _presentation;
		public IPresentation CurrentPresentation
		{
			get
			{
				return _presentation;
			}
			protected set
			{
				if (_presentation != value)
				{
					_presentation = value;
				}
			}
		}

		public void ChangePresentation(IPresentation presentation)
		{
			if (presentation != CurrentPresentation)
			{
				//1. Detach the current presentation.
				if (CurrentPresentation != null)
				{
					CurrentPresentation.Detach();
				}

				//2. Load the new theme.
				if (presentation != null)
				{
					presentation.Attach();
				}

				//3. Restore the current theme.
				CurrentPresentation = presentation;
			}
		}

		public void ChangePresentation(string presentationName)
		{
			if (Presentations != null)
			{
				var presentation = Presentations.FirstOrDefault(i => string.Equals(i.Name, presentationName));
				if (presentation != null)
				{
					ChangePresentation(presentation);
				}
			}
		}

		#endregion
	}
}
