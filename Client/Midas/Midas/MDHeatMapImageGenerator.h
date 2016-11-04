//
//  MDImageGenerator.h
//  Midas
//
//  Created by Thomas Günzel on 10/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import UIKit;

#import "MDImageGenerator.h"

@class MDTouchSequence;

@interface MDHeatMapImageGenerator : NSObject<MDImageGenerator>

+(UIImage*)imageForTouchSequences:(NSArray<MDTouchSequence*>*)touchSequences;

@end
