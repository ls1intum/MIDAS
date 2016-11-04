//
//  MDFilterBubblesViewController.m
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 02/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFilterBubblesViewController.h"
#import "MDFBNavigationView.h"

#import "MDFilter.h"
#import "MDFilterGroup.h"
#import "MDFilterSimple.h"
#import "MDFilterValue.h"

#import "MDFilterViewController.h"

#import "UIButton+MDCircleButton.h"

@interface MDFilterBubblesViewController ()

@property(readwrite,nonatomic,strong) UIScrollView *scrollView;
@property(readwrite,nonatomic,strong) UIStackView *contentStackView;


@end

@implementation MDFilterBubblesViewController

- (instancetype)initWithFilter:(MDFilter *)filter {
	self = [super init];
	if (self) {
		_filterName = nil;
		_filter = filter;
	}
	return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view.
	_scrollView = [[UIScrollView alloc] initWithFrame:self.view.bounds];
	_scrollView.translatesAutoresizingMaskIntoConstraints = NO;
	//_scrollView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
	_scrollView.contentInset = UIEdgeInsetsMake(0.0, 10.0, 0.0, 10.0);
	_scrollView.bounces = YES;
	_scrollView.alwaysBounceHorizontal = YES;
	[self.view addSubview:_scrollView];
	
	
	_contentStackView = [[UIStackView alloc] initWithArrangedSubviews:@[]];
	_contentStackView.translatesAutoresizingMaskIntoConstraints = NO;
	_contentStackView.alignment = UIStackViewAlignmentFill;
	_contentStackView.distribution = UIStackViewDistributionFillEqually;
	_contentStackView.axis = UILayoutConstraintAxisHorizontal;
	
	if([_filter isKindOfClass:[MDFilterGroup class]]) {
		MDFilterGroup *fGroup = (MDFilterGroup*)_filter;
		
		NSUInteger idx = 0;
		for (MDFilter *filter in fGroup.filters) {
			UIButton *btn = [UIButton circleButtonWithTitle:filter.name image:filter.image];
			btn.titleLabel.font = [UIFont systemFontOfSize:12.0];
			btn.tag = idx;
			
			[btn addTarget:self action:@selector(showSubfilters:) forControlEvents:UIControlEventTouchUpInside];
			[_contentStackView addArrangedSubview:btn];
			if(idx == 0){
				[btn.widthAnchor constraintEqualToConstant:80.0].active = YES;
			}
			idx++;
		}
	} else if([_filter isKindOfClass:[MDFilterSimple class]]) {
		MDFilterSimple *simpleFilter = (MDFilterSimple*)_filter;
		
		NSUInteger idx = 0;
		for (MDFilterValue *value in simpleFilter.availableValues) {
			UIButton *btn = [UIButton circleButtonWithTitle:value.name image:value.image];
			btn.titleLabel.font = [UIFont systemFontOfSize:12.0];
			btn.tag = idx;
			[btn centerVertically];
			[btn addTarget:self action:@selector(toggleInclusion:) forControlEvents:UIControlEventTouchUpInside];
			[_contentStackView addArrangedSubview:btn];
			if(idx == 0){
				[btn.widthAnchor constraintEqualToConstant:80.0].active = YES;
			}
			idx++;
		}
	}
	
	[self.scrollView addSubview:_contentStackView];
	
	[_scrollView.topAnchor constraintEqualToAnchor:self.view.topAnchor].active = YES;
	[_scrollView.trailingAnchor constraintEqualToAnchor:self.view.trailingAnchor].active = YES;
	[_scrollView.bottomAnchor constraintEqualToAnchor:self.view.bottomAnchor].active = YES;
	[_scrollView.leadingAnchor constraintEqualToAnchor:self.view.leadingAnchor].active = YES;
	
	[_scrollView.heightAnchor constraintEqualToAnchor:_contentStackView.heightAnchor multiplier:1.0].active = YES;
//	[_scrollView.widthAnchor constraintEqualToAnchor:_contentStackView.widthAnchor multiplier:1.0].active = YES;
	[_scrollView.bottomAnchor constraintEqualToAnchor:_contentStackView.bottomAnchor].active = YES;
	[_scrollView.topAnchor constraintEqualToAnchor:_contentStackView.topAnchor].active = YES;
	[_scrollView.leadingAnchor constraintEqualToAnchor:_contentStackView.leadingAnchor].active = YES;
	[_scrollView.trailingAnchor constraintEqualToAnchor:_contentStackView.trailingAnchor].active = YES;
//	[_scrollView.trai constraintEqualToAnchor:_scrollView.leadingAnchor].active = YES;
	
//	[_navigationView.trailingAnchor constraintEqualToAnchor:_scrollView.leadingAnchor].active = YES;
}

-(void)viewWillAppear:(BOOL)animated {
	[super viewWillAppear:animated];
	[self updateActive];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

/*-(void)viewWillLayoutSubviews {
	[super viewWillLayoutSubviews];
	
	for (UIButton *btn in _contentStackView.arrangedSubviews) {
		if([btn isKindOfClass:[UIButton class]] == NO) {
			continue;
		}
		[btn centerVertically];
	}
}*/

-(void)showSubfilters:(UIButton*)sender {
	NSUInteger tag = sender.tag;
	NSArray *filters = [((MDFilterGroup*)_filter) filters];
	MDFilter *f = [filters objectAtIndex:tag];
	
	NSLog(@"Did Tap: %@",f.name);
	if([self.parentViewController isKindOfClass:[MDFilterViewController class]]) {
		MDFilterViewController *fvc = (MDFilterViewController *)self.parentViewController;
		MDFilterBubblesViewController *subVC = [[MDFilterBubblesViewController alloc] initWithFilter:f];
		[fvc pushViewController:subVC animated:YES];
	}
}

-(void)toggleInclusion:(UIButton*)sender {
	NSUInteger tag = sender.tag;
	MDFilterSimple *filter = ((MDFilterSimple*)_filter);
	NSArray *filters = [filter availableValues];
	MDFilterValue *f = [filters objectAtIndex:tag];
	
	BOOL accepted = [filter value:f.acceptedValue isAcceptedForFilterNamed:_filterName];
	if(accepted) {
		[filter removeAcceptedValue:f.acceptedValue forFilterNamed:_filterName];
		[sender setIncluded:NO];
	} else {
		[filter addAcceptedValue:f.acceptedValue forFilterNamed:_filterName];
		[sender setIncluded:YES];
	}
	
	NSLog(@"Did Tap Value: %@",f.name);
}

-(void)updateActive {
	if([_filter isKindOfClass:[MDFilterGroup class]]) {
		MDFilterGroup *fGroup = (MDFilterGroup*)_filter;
		NSArray<MDFilter*> *filters = fGroup.filters;
		for (UIButton* btn in self.contentStackView.arrangedSubviews) {
			MDFilter *f = [filters objectAtIndex:btn.tag];
			[btn setIncluded:[f activeForFilterNamed:_filterName]];
		}
	} else if([_filter isKindOfClass:[MDFilterSimple class]]) {
		MDFilterSimple *filter = (MDFilterSimple*)_filter;
		NSArray *avail = filter.availableValues;
		for (UIButton* btn in self.contentStackView.arrangedSubviews) {
			if(_filterName == nil) {
				[btn setIncluded:NO];
				btn.enabled = NO;
				continue;
			}
			btn.enabled = YES;
			MDFilterValue *val = [avail objectAtIndex:btn.tag];
			BOOL acc = [filter value:val.acceptedValue isAcceptedForFilterNamed:_filterName];
			[btn setIncluded:acc];
		}
	}
}


-(void)pop:(id)sender {
	if([self.parentViewController isKindOfClass:[MDFilterViewController class]]) {
		MDFilterViewController *fvc = (MDFilterViewController *)self.parentViewController;
		[fvc popViewControllerAnimated:YES];
	}
}


-(void)setFilterName:(NSString *)filterName {
	_filterName = filterName;
	[self updateActive];
}

-(void)reset {
	[_filter resetForFilterNamed:_filterName];
	[self updateActive];
}

@end
