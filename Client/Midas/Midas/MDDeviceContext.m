//
//  MDDeviceContext.m
//  Midas
//
//  Created by Thomas Günzel on 28/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDeviceContext.h"

#import "MDEncodeTypes.h"

#import <sys/utsname.h> // import it in your header or implementation file.

NSString* deviceName()
{
	struct utsname systemInfo;
	uname(&systemInfo);
	
	return [NSString stringWithCString:systemInfo.machine
							  encoding:NSUTF8StringEncoding];
}

typedef NS_ENUM(uint8_t, MD_UIUserInterfaceIdiom) {
	MD_UIUserInterfaceIdiom_Unspecified = 0,
	MD_UIUserInterfaceIdiom_Phone,
	MD_UIUserInterfaceIdiom_Pad,
	MD_UIUserInterfaceIdiom_TV,
	MD_UIUserInterfaceIdiom_CarPlay,
};

MD_UIUserInterfaceIdiom ui_to_mdUserInterfaceIdiom(UIUserInterfaceIdiom idiom) {
	switch (idiom) {
		case UIUserInterfaceIdiomPhone:
			return MD_UIUserInterfaceIdiom_Phone;
		case UIUserInterfaceIdiomPad:
			return MD_UIUserInterfaceIdiom_Pad;
		case UIUserInterfaceIdiomTV:
			return MD_UIUserInterfaceIdiom_TV;
		case UIUserInterfaceIdiomCarPlay:
			return MD_UIUserInterfaceIdiom_CarPlay;
		case UIUserInterfaceIdiomUnspecified: // NOTE: FALLTHROUGH
		default:
			return MD_UIUserInterfaceIdiom_Unspecified;
	}
	return MD_UIUserInterfaceIdiom_Unspecified;
}


@implementation MDDeviceContext

-(instancetype)init {
	self = [super init];
	if (self) {
		UIDevice *device = [UIDevice currentDevice];
		_model = deviceName();
		_systemName = [device systemName];
		_systemVersion = [device systemVersion];
		_appVersion = [[NSBundle mainBundle] objectForInfoDictionaryKey:(NSString *)kCFBundleVersionKey];
		
		_userInterfaceIdiom = [device userInterfaceIdiom];
	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	size_t model_size = MIN(_model.length,254);
	size_t system_name_size = MIN(_systemName.length,254);
	size_t system_version_size = MIN(_systemVersion.length,254);
	size_t app_version_size = MIN(_appVersion.length,254);
	size_t dim_size = sizeof(uint16_t) *2;
	
	size_t data_size = (6 * sizeof(uint8_t)) + dim_size + model_size + system_name_size + system_version_size + app_version_size;
	uint8_t *data = malloc(data_size);
	
	data[0] = kMDEncoded_Context_Device;
	data[1] = ui_to_mdUserInterfaceIdiom(_userInterfaceIdiom);
	
	size_t pos = 2;
	
	// Screen
	CGRect screen = [[UIScreen mainScreen] bounds];
	uint16_t w = (uint16_t)screen.size.width;
	uint16_t h = (uint16_t)screen.size.height;
	uint16_t dim[2] = {w, h};
	memcpy(&data[pos], &dim[0], dim_size);
	pos += dim_size;

	// Model
	data[pos] = model_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _model.UTF8String, model_size);
	pos += model_size;
	
	// System Name
	data[pos] = system_name_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _systemName.UTF8String, system_name_size);
	pos += system_name_size;
	
	// System Version
	data[pos] = system_version_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _systemVersion.UTF8String, system_version_size);
	pos += system_version_size;
	
	// App Version
	data[pos] = app_version_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _appVersion.UTF8String, app_version_size);
	pos += app_version_size;
	
	[encoder writeData:data ofLength:data_size];
}

@end
