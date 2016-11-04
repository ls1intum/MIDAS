//
//  MDCircledImageButton.m
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 02/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDCircledImageButton.h"

CGFloat const kTextTopPadding = 10.0;

@interface MDCircledImageButton()

@property(readwrite,nonatomic,strong) CALayer *imageLayer;
@property(readwrite,nonatomic,strong) UILabel *label;

@end

@implementation MDCircledImageButton

- (instancetype)initWithTitle:(NSString *)title image:(UIImage *)image {
	self = [super init];
	if (self) {
		[self setup];
		self.title = title;
		self.image = image;
	}
	return self;
}

- (instancetype)initWithCoder:(NSCoder *)coder
{
	self = [super initWithCoder:coder];
	if (self) {
		[self setup];
		self.title = @"Bottle";
		self.image = [UIImage imageNamed:@"reset"];
	}
	return self;
}

- (instancetype)initWithFrame:(CGRect)frame
{
	self = [super initWithFrame:frame];
	if (self) {
		[self setup];
	}
	return self;
}

-(void)setup {
	_imageLayer = [CALayer layer];
	_imageLayer.borderColor = [UIColor whiteColor].CGColor;
	_imageLayer.borderWidth = 2.0;
	_imageLayer.masksToBounds = YES;
	[self.layer addSublayer:_imageLayer];
	
	_label = [[UILabel alloc] init];
	_label.textColor = [UIColor whiteColor];
	_label.textAlignment = NSTextAlignmentCenter;
	[self addSubview:_label];
}

-(void)layoutSubviews {
	[super layoutSubviews];
	
	CGRect iFrame = self.bounds;
	iFrame.size.height = iFrame.size.width;
	_imageLayer.cornerRadius = (iFrame.size.height / 2.0);
	_imageLayer.frame = iFrame;
	
	CGRect lFrame = self.bounds;
	lFrame.origin.y = iFrame.size.height + 4.0;
	CGSize ls = [_label systemLayoutSizeFittingSize:CGSizeMake(CGFLOAT_MAX, CGFLOAT_MAX)];
	lFrame.size.height = ls.height;
	_label.frame = lFrame;
	
}

-(void)setHighlighted:(BOOL)highlighted {
	[super setHighlighted:highlighted];
	if(self.highlighted == YES) {
		self.alpha = 0.5;
	} else {
		self.alpha = 1.0;
	}
}

-(void)setImage:(UIImage *)image {
	if(_image == image) {
		return;
	}
	
	_image = image;
	_imageLayer.contents = (id)_image.CGImage;
}

-(void)setTitle:(NSString *)title {
	if(_title == title) {
		return;
	}
	
	_title = title;
	_label.text = _title;
	[self invalidateIntrinsicContentSize];
}

-(CGSize)intrinsicContentSize {
	CGSize s;
	CGSize bs = self.bounds.size;
	CGSize ls = [_label systemLayoutSizeFittingSize:CGSizeMake(CGFLOAT_MAX, CGFLOAT_MAX)];
	s = bs;
	CGFloat max_dim = fmax(10.0,fmax(bs.height, bs.width));
	max_dim = fmax(max_dim, ls.width);
	s.height = max_dim + 4.0 + ls.height;
	s.width = max_dim;
	
	
	return s;
}

-(CGFloat)borderWidth {
	return _imageLayer.borderWidth;
}

-(void)setBorderWidth:(CGFloat)borderWidth {
	_imageLayer.borderWidth = borderWidth;
}

@end
