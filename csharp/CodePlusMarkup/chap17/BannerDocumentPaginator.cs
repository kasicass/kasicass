using System;
using System.Globalization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;

namespace Petzold.PrintBanner
{
	public class BannerDocumentPaginator : DocumentPaginator
	{
		string txt = "";
		Typeface face = new Typeface("");
		Size sizePage;
		Size sizeMax = new Size(0, 0);

		public string Text
		{
			set { txt = value; }
			get { return txt; }
		}

		public Typeface Typeface
		{
			set { face = value; }
			get { return face; }
		}

		FormattedText GetFormattedText(char ch, Typeface face, double em)
		{
			return new FormattedText(ch.ToString(), CultureInfo.CurrentCulture, FlowDirection.LeftToRight,
				face, em, Brushes.Black);
		}

		public override bool IsPageCountValid
		{
			get
			{
				foreach (char ch in txt)
				{
					FormattedText formtxt = GetFormattedText(ch, face, 100);
					sizeMax.Width = Math.Max(sizeMax.Width, formtxt.Width);
					sizeMax.Height = Math.Max(sizeMax.Height, formtxt.Height);
				}
				return true;
			}
		}

		public override int PageCount
		{
			get { return txt == null ? 0 : txt.Length; }
		}

		public override Size PageSize
		{
			set { sizePage = value; }
			get { return sizePage; }
		}

		public override DocumentPage GetPage(int numPage)
		{
			DrawingVisual vis = new DrawingVisual();
			DrawingContext dc = vis.RenderOpen();

			double factor = Math.Min((PageSize.Width-96)/sizeMax.Width, (PageSize.Height-96)/sizeMax.Height);
			FormattedText formtxt = GetFormattedText(txt[numPage], face, factor*100);

			Point ptText = new Point((PageSize.Width - formtxt.Width)/2, (PageSize.Height - formtxt.Height)/2);
			dc.DrawText(formtxt, ptText);
			dc.Close();

			return new DocumentPage(vis);
		}

		public override IDocumentPaginatorSource Source
		{
			get { return null; }
		}
	}
}

