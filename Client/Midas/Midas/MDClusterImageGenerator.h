//
//  MDClusterImageGenerator.h
//  Midas
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDImageGenerator.h"

@interface MDClusterImageGenerator : NSObject<MDImageGenerator>

+(UIImage*)imageForTouchSequences:(NSArray<MDTouchSequence*>*)touchSequences;

@end
