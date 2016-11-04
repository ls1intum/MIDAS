//
//  MDSegmentedControlItem.m
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDSegmentedControlItem.h"

CGFloat const kColorStripeHeight = 4.0;

@interface MDSegmentedControlItem()

@property(readwrite,nonatomic,strong) UILabel *label;
@property(readwrite,nonatomic,strong) CALayer *colorLayer;

@end

@implementation MDSegmentedControlItem


- (instancetype)initWithTitle:(NSString *)title color:(UIColor *)color {
	self = [super init];
	if (self) {
		[self setup];
		self.title = title;
		self.color = color;
	}
	return self;
}

-(void)setup {
	_label = [[UILabel alloc] init];
	_label.textColor = [UIColor whiteColor];
	_label.textAlignment = NSTextAlignmentCenter;
	_colorLayer = [CALayer layer];
	
	_active = YES;
	self.active = NO;
	
	[self addSubview:_label];
	[self.layer addSublayer:_colorLayer];
}

-(void)layoutSubviews {
	[super layoutSubviews];
	
	CGRect cFrame = self.bounds;
	cFrame.size.height = kColorStripeHeight;
	_colorLayer.frame = cFrame;
	
	CGRect lFrame = self.bounds;
	lFrame.size.height -= kColorStripeHeight;
	lFrame.origin.y += kColorStripeHeight;
	_label.frame = lFrame;
}

-(void)setTitle:(NSString *)title {
	if(_title == title) {
		return;
	}
	
	_title = title;
	_label.text = title;
}

-(void)setColor:(UIColor *)color {
	if (_color == color) {
		return;
	}
	_color = color;
	_colorLayer.backgroundColor = color.CGColor;
}

-(void)setActive:(BOOL)active {
	if(_active == active) {
		return;
	}
	
	_active = active;
	
	_label.enabled = _active;
	_colorLayer.opacity = (_active == YES ? 1.0 : 0.25);
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect {
    // Drawing code
}
*/

@end
