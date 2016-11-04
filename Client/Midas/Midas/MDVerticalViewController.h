//
//  MDVerticalViewController.h
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface MDVerticalViewController : UIViewController

-(instancetype)initWithRootViewController:(UIViewController*)rootViewController;

@property(readwrite,nonatomic,strong) NSArray<UIViewController*> *viewControllers;

-(void)setViewControllers:(NSArray<UIViewController *> *)viewControllers animated:(BOOL)animated;

-(void)pushViewController:(UIViewController*)viewController animated:(BOOL)animated;
-(void)popViewControllerAnimated:(BOOL)animated;

@end
