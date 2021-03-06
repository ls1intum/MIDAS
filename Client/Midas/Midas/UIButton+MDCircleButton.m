//
//  UIButton+MDCircleButton.m
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "UIButton+MDCircleButton.h"

#import "MDCircleButton.h"

@implementation UIButton (MDCircleButton)

+(instancetype)circleButtonWithTitle:(NSString *)title image:(UIImage *)image {
	/*UIButton *btn = [UIButton buttonWithType:UIButtonTypeCustom];
	
	[btn setTitle:title forState:UIControlStateNormal];
	[btn setImage:image forState:UIControlStateNormal];
	
	CALayer *imgLyr = btn.imageView.layer;
	imgLyr.borderWidth = 1.0;
	imgLyr.borderColor = [UIColor whiteColor].CGColor;
	imgLyr.masksToBounds = YES;
	
	btn.titleLabel.numberOfLines = 0;*/
	MDCircleButton *btn = [[MDCircleButton alloc] initWithTitle:title image:image];
	
	return btn;
}
/*
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
*/
@end
