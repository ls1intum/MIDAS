//
//  MDVerticalViewController.m
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDVerticalViewController.h"

@interface MDVerticalViewController ()

//@property(readwrite,nonatomic,strong) NSMutableArray<UIViewController*> *vcStack;

@end

@implementation MDVerticalViewController

- (instancetype)initWithRootViewController:(UIViewController *)rootViewController {
	self = [super init];
	if (self) {
		self.viewControllers = [NSArray arrayWithObject:rootViewController];
	}
	return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view.
	self.view.clipsToBounds = YES;
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

-(void)setViewControllers:(NSArray<UIViewController *> *)viewControllers {
	[self setViewControllers:viewControllers animated:NO];
}

-(void)setViewControllers:(NSArray<UIViewController *> *)viewControllers animated:(BOOL)animated {
	[self setViewControllers:viewControllers animated:animated push:YES];
}

-(void)setViewControllers:(NSArray<UIViewController *> *)viewControllers animated:(BOOL)animated push:(BOOL)push {
	if(_viewControllers == viewControllers) {
		return;
	}
	
	UIViewController *lastTop = _viewControllers.lastObject;
	
	[self willChangeValueForKey:@"viewControllers"];
	_viewControllers = viewControllers;
	[self didChangeValueForKey:@"viewControllers"];
	
	UIViewController *newTop = _viewControllers.lastObject;
	
	if(lastTop == newTop) {
		return;
	}
	
	newTop.view.frame = self.view.bounds;
	newTop.view.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;

	if(animated && lastTop && newTop) {
		[lastTop willMoveToParentViewController:nil];
		[self addChildViewController:newTop];
		
		CGRect lastFrameBefore = lastTop.view.frame;
		CGRect newFrameBefore = lastFrameBefore;
		newFrameBefore.origin.y = -newFrameBefore.size.height;
		CGRect lastFrameAfter = lastFrameBefore;
		lastFrameAfter.origin.y += lastFrameAfter.size.height;
		
		if(push) {
			newTop.view.frame = newFrameBefore;
		} else {
			newTop.view.frame = lastFrameAfter;
		}
		[self.view addSubview:newTop.view];
		
		[UIView animateWithDuration:0.15 animations:^{
			if(push) {
				lastTop.view.frame = lastFrameAfter;
			} else {
				lastTop.view.frame = newFrameBefore;
			}
			newTop.view.frame = lastFrameBefore;
		} completion:^(BOOL finished) {
			[lastTop.view removeFromSuperview];
			[lastTop removeFromParentViewController];
			[newTop didMoveToParentViewController:self];
		}];
		
		//[UIView transitionFromView:lastTop.view toView:newTop.view duration:0.15 options:0 completion:^(BOOL finished) {
		//}];
	} else {
		if(lastTop) {
			[lastTop willMoveToParentViewController:nil];
			[lastTop.view removeFromSuperview];
			[lastTop removeFromParentViewController];
		}
		
		if(newTop) {
			[self addChildViewController:newTop];
			[self.view addSubview:newTop.view];
			[newTop didMoveToParentViewController:self];
		}
	}
	
}

-(void)pushViewController:(UIViewController*)viewController animated:(BOOL)animated {
	[self setViewControllers:[_viewControllers arrayByAddingObject:viewController] animated:animated];
}

-(void)popViewControllerAnimated:(BOOL)animated {
	if(_viewControllers.count <= 1) {
		return;
	}
	[self setViewControllers:[_viewControllers subarrayWithRange:NSMakeRange(0, _viewControllers.count-1)] animated:YES push:NO];
}


/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/

@end
