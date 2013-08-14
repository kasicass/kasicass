using System;
using System.Globalization;
using System.Windows;
using System.Windows.Data;

namespace Petzold.ExploreDependencyProperties
{
	class MetadataToFlags : IValueConverter
	{
		public object Convert(object obj, Type type, object param, CultureInfo culture)
		{
			FrameworkPropertyMetadataOptions flags = 0;
			FrameworkPropertyMetadata metadata = obj as FrameworkPropertyMetadata;
			if (metadata == null)
				return null;

			if (metadata.AffectsMeasure)
				flags |= FrameworkPropertyMetadataOptions.AffectsMeasure;

			if (metadata.AffectsArrange)
				flags |= FrameworkPropertyMetadataOptions.AffectsArrange;

			if (metadata.AffectsParentMeasure)
				flags |= FrameworkPropertyMetadataOptions.AffectsParentMeasure;

			if (metadata.AffectsParentArrange)
				flags |= FrameworkPropertyMetadataOptions.AffectsParentArrange;

			if (metadata.AffectsRender)
				flags |= FrameworkPropertyMetadataOptions.AffectsRender;

			if (metadata.Inherits)
				flags |= FrameworkPropertyMetadataOptions.Inherits;

			if (metadata.OverridesInheritanceBehavior)
				flags |= FrameworkPropertyMetadataOptions.OverridesInheritanceBehavior;

			if (metadata.IsNotDataBindable)
				flags |= FrameworkPropertyMetadataOptions.NotDataBindable;

			if (metadata.BindsTwoWayByDefault)
				flags |= FrameworkPropertyMetadataOptions.BindsTwoWayByDefault;

			if (metadata.Journal)
				flags |= FrameworkPropertyMetadataOptions.Journal;

			return flags;
		}

		public object ConvertBack(object obj, Type type, object param, CultureInfo culture)
		{
			return new FrameworkPropertyMetadata(null, (FrameworkPropertyMetadataOptions)obj);
		}
	}
}

