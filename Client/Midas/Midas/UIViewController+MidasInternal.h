//
//  UIViewController+Midas.h
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "UIViewController+Midas.h"


@interface UIViewController (MidasInternal)

+(void)md_swizzle;
+(void)md_swizzleViewDidAppear;
+(void)md_swizzlePrepareForSegue;
-(void)md_logPrepareForSegue:(UIStoryboardSegue*)segue sender:(id)sender;

-(NSString*)md_keyForSender:(id)sender;

@end
