using System.Collections;
using System.Collections.Generic;
using UnityEngine;
public sealed class App
{
	public const string key="BIT.GAME.X.8.8.8.8";
	public Game game;
	public int uid;
	public string exchange_accid;
	public string wallet_addr;
	public string token;
	public float role_balance;
	public float user_balance;
	public float exchange_balance;
	private static volatile App instance;
	private static Object syncRootObject = new Object();

	public static App Instance{
		get{
			if (instance == null){
				lock (syncRootObject){
					if (instance == null){
						instance = new App();
					}
				}
			}
			return instance;
		}
	}

	public void Init(){
		this.game = new Game();
	}

	public string device_id{
		get{
			return SystemInfo.deviceUniqueIdentifier;
		}
	}

	public long time{
		get{
			var startTime = System.TimeZone.CurrentTimeZone.ToLocalTime(new System.DateTime(1970, 1, 1)); // 当地时区
			var timeStamp = (long)(System.DateTime.Now - startTime).TotalSeconds; // 相差秒数
			return timeStamp;
		}
	}

	public string device_model{
		get{
			return SystemInfo.deviceModel;
		}
	}

	public string os_type{
		get{
			return SystemInfo.operatingSystem;
		}
	}

	public string os_ver{
		get{
			return SystemInfo.operatingSystem;
		}
	}

	public string lang{
		get{
			return Application.systemLanguage.ToString();
		}
	}

	public string org_device_id{
		get{
			return device_id;
		}
	}

	public string gc_id{
		get{
			return "";
		}
	}

	public string fb_id{
		get{
			return "";
		}
	}
}
