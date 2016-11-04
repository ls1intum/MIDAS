//
//  MDPercentLabel.m
//  Midas
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDPercentLabel.h"

@implementation MDPercentLabel

- (instancetype)initWithFrame:(CGRect)frame {
	self = [super initWithFrame:frame];
	if (self) {
		UIColor *darkGreen = [UIColor colorWithRed:0.0 green:0.5 blue:0.0 alpha:1.0];
		self.layer.backgroundColor = [darkGreen colorWithAlphaComponent:0.5].CGColor;
//		self.layer.cornerRadius = 4.0;
		self.layer.borderWidth = 1.0;
		self.layer.borderColor = darkGreen.CGColor;
		self.textAlignment = NSTextAlignmentCenter;
		self.textColor = [UIColor whiteColor];
		self.numberOfLines = 1;
		self.font = [UIFont systemFontOfSize:14.0];
		self.minimumScaleFactor = 0.5;
	}
	return self;
}


-(void)updateLabel {
	if(_allActionsCount > 0) {
		CGFloat percent = ((CGFloat)_actionsCount / (CGFloat)_allActionsCount) * 100.0;
		self.text = [NSString stringWithFormat:@"%.1f%%",percent];
	} else {
		self.text = @"NaN";
	}
}

@end
