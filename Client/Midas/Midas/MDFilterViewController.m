//
//  MDFilterViewController.m
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFilterViewController.h"
#import "MDFilterBubblesViewController.h"

#import "MDFilter.h"
#import "MDFilterGroup.h"

@interface MDFilterViewController ()

@end

@implementation MDFilterViewController

- (instancetype)initWithFilter:(MDFilter *)filter {
	MDFilterBubblesViewController *rootBubbles = [[MDFilterBubblesViewController alloc] initWithFilter:filter];
	
	self = [super initWithRootViewController:rootBubbles];
	if (self) {
		_filter = filter;
	}
	return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view.
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

-(void)setFilterName:(NSString *)filterName {
	if(_filterName == filterName) {
		return;
	}
	NSLog(@"Changed to filter: %@",filterName);
	_filterName = filterName;
	for (MDFilterBubblesViewController *vc in self.viewControllers) {
		vc.filterName = filterName;
	}
}

-(void)pushViewController:(MDFilterBubblesViewController *)viewController animated:(BOOL)animated {
	viewController.filterName = _filterName;
	[super pushViewController:viewController animated:animated];
}

-(void)back:(id)sender {
	[self popViewControllerAnimated:YES];
}

-(void)reset:(id)sender {
	MDFilterBubblesViewController *fbvc = (MDFilterBubblesViewController *)[self.viewControllers lastObject];
	
	[fbvc reset];
}

@end
