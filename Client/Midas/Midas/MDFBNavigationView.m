//
//  MDFBNavigationView.m
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 02/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFBNavigationView.h"
#import "MDCircledImageButton.h"

#import "UIButton+MDCircleButton.h"

@interface MDFBNavigationView()

@property(readwrite,nonatomic,strong) CAGradientLayer *dividerLayer;
@property(readwrite,nonatomic,strong) UIStackView *stackView;
@end

@implementation MDFBNavigationView

- (instancetype)init {
	self = [super init];
	if (self) {
		[self setup];
	}
	return self;
}


-(void)setup {
	_stackView = [[UIStackView alloc] init];
	_stackView.axis = UILayoutConstraintAxisVertical;
	_stackView.distribution = UIStackViewDistributionFillEqually;
	
	NSBundle *bundle = [NSBundle bundleWithIdentifier:@"de.tum.in.www1.tg.midas"];
	
	_resetButton = [UIButton circleButtonWithTitle:@"Reset" image:[UIImage imageNamed:@"reset" inBundle:bundle compatibleWithTraitCollection:nil]];
	_resetButton.titleLabel.font = [UIFont systemFontOfSize:12.0];
	[_stackView addArrangedSubview:_resetButton];
	
	_backButton = [UIButton circleButtonWithTitle:@"Back" image:[UIImage imageNamed:@"back" inBundle:bundle compatibleWithTraitCollection:nil]];
	_backButton.titleLabel.font = [UIFont systemFontOfSize:12.0];
	[_stackView addArrangedSubview:_backButton];
	
	_stackView.translatesAutoresizingMaskIntoConstraints = NO;
	
	[self addSubview:_stackView];
	
	[self.topAnchor constraintEqualToAnchor:_stackView.topAnchor].active = YES;
	[self.bottomAnchor constraintEqualToAnchor:_stackView.bottomAnchor].active = YES;
	[self.leftAnchor constraintEqualToAnchor:_stackView.leftAnchor].active = YES;
	[self.rightAnchor constraintEqualToAnchor:_stackView.rightAnchor].active = YES;
	
	_dividerLayer = [CAGradientLayer layer];
	CGColorRef whiteRef = [UIColor whiteColor].CGColor;
	CGColorRef clearRef = [[UIColor whiteColor] colorWithAlphaComponent:0.0].CGColor;
	_dividerLayer.colors = @[(__bridge id)clearRef,(__bridge id)whiteRef,(__bridge id)clearRef];
	
	[self.layer addSublayer:_dividerLayer];
}


-(void)layoutSubviews {
	[super layoutSubviews];
	
	CGRect divFrame = self.bounds;
	divFrame.origin.y += 10.0;
	divFrame.size.height -= 20.0;
	divFrame.origin.x = divFrame.size.width - 1.0;
	divFrame.size.width = 1.0;
	_dividerLayer.frame = divFrame;
}

//-(CGSize)intrinsicContentSize {
//	CGSize bs = self.bounds.size;
//	bs.width = 24.0;
//	return bs;
//}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect {
    // Drawing code
}
*/

@end
