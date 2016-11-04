//
//  UIControl+Midas.h
//  Midas
//
//  Created by Thomas Günzel on 18/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "UIControl+Midas.h"

@interface UIControl (MidasInternal)

+(void)md_swizzleSend;

-(void)md_sendAction:(SEL)action to:(id)target forEvent:(UIEvent*)event;
-(void)md_sendActionsForControlEvents:(UIControlEvents)events;
@end
