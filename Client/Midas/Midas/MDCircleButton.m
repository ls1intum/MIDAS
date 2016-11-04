//
//  MDCircleButton.m
//  Midas
//
//  Created by Thomas Günzel on 09/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDCircleButton.h"

@implementation MDCircleButton

-(instancetype)initWithTitle:(NSString *)title image:(UIImage *)image {
	self = [super initWithFrame:CGRectMake(0, 0, 44, 44)];
	if (self) {		
		[self setTitle:title forState:UIControlStateNormal];
		[self setImage:image forState:UIControlStateNormal];
		
		CALayer *imgLyr = self.imageView.layer;
		imgLyr.borderWidth = 1.0;
		imgLyr.borderColor = [UIColor whiteColor].CGColor;
		imgLyr.masksToBounds = YES;
		
		self.titleLabel.numberOfLines = 0;
	}
	return self;
}

- (void)centerVerticallyWithPadding:(float)padding
{
	CGSize imageSize = self.imageView.frame.size;
	CGSize titleSize = self.titleLabel.frame.size;
	
	CGFloat totalHeight = (imageSize.height + titleSize.height + padding);
	
	self.imageEdgeInsets = UIEdgeInsetsMake(- (totalHeight - imageSize.height),
											0.0f,
											0.0f,
											- titleSize.width);
	
	self.titleEdgeInsets = UIEdgeInsetsMake(0.0f,
											- imageSize.width,
											- (totalHeight - titleSize.height),
											0.0f);
	
	self.imageView.layer.cornerRadius = self.imageView.bounds.size.width/2.0;
}


- (void)centerVertically
{
	const CGFloat kDefaultPadding = 6.0f;
	
	[self centerVerticallyWithPadding:kDefaultPadding];
}

-(void)layoutSubviews {
	[super layoutSubviews];
	[self centerVertically];
}


-(void)setIncluded:(BOOL)included {
	if(included == YES) {
		self.imageView.layer.borderColor = [UIColor greenColor].CGColor;
	} else {
		self.imageView.layer.borderColor = [UIColor whiteColor].CGColor;
	}
}

-(BOOL)isIncluded {
	return self.imageView.layer.borderColor == [UIColor greenColor].CGColor;
}

@end
