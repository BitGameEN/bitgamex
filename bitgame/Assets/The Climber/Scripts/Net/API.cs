using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class API{
	private static API _instance;
	public static API Instance{
		get{
			if(_instance == null){
				_instance = new API();
			}
			return _instance;
		}
	}
	public void LoginGame(Action success,Action fail){
		var args = new Dictionary<string,object>();
		args["a"] = "login_game";
		args["uid"] = 0;
		args["game_id"] = App.Instance.game.game_id;
		args["device_id"] = App.Instance.device_id;
		args["time"] = App.Instance.time;
		args["device_model"] = App.Instance.device_model;
		args["os_type"] = App.Instance.os_type;
		args["os_ver"] = App.Instance.os_ver;
		args["lang"] = App.Instance.lang;
		args["org_device_id"] = App.Instance.org_device_id;
		args["gc_id"] = App.Instance.gc_id;
		args["fb_id"] = App.Instance.fb_id;
		args["sign"] = Encrypt.Md5(0+""+App.Instance.game.game_id+""+App.Instance.device_id+""+args["time"]+""+App.key);
		Server.Instance.Get<ServerVO.LoginGameVO>(args,(receive)=>{
			App.Instance.uid = receive.uid;
			App.Instance.token = receive.token;
			App.Instance.game.data = receive.game_data;
			App.Instance.balance = receive.balance;
			success();
		},()=>{
			fail();
		});
	}

	public void GetGame(Action success,Action fail){
		var args = new Dictionary<string,object>();
		args["a"] = "get_game";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		Server.Instance.Get<ServerVO.GetGameVO>(args,(receive)=>{
			App.Instance.game.data = receive.game_data;
			App.Instance.balance = receive.balance;
			success();
		},()=>{
			fail();
		});
	}

	public void SaveGame(Action success,Action fail){
		var args = new Dictionary<string,object>();
		args["a"] = "save_game";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["game_data"] = App.Instance.game.data;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+App.Instance.game.data+args["time"]+App.key);
		Server.Instance.Post<ServerVO.SaveGameVO>(args,(receive)=>{
			App.Instance.game.data = receive.game_data;
			App.Instance.balance = receive.balance;
			Debug.LogWarning("f_res"+receive.f_res);
			success();
		},()=>{
			fail();
		});
	}

	public void TransferCoinInGame(int toId,float amount,Action success,Action fail){
		var args = new Dictionary<string,object>();
		args["a"] = "transfer_coin_in_game";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["dst_uid"] = toId;
		args["amount"] = amount;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+
		App.Instance.game.game_id+""+App.Instance.token+""+toId+""+amount+""+args["time"]+App.key);
		Server.Instance.Get<ServerVO.TransferCoinInGameVO>(args,(receive)=>{
			App.Instance.balance = receive.balance;
			success();
		},()=>{
			fail();
		});
	}

	public void BindExchangeAccid(string accid,Action success,Action fail){
		var args = new Dictionary<string,object>();
		args["a"] = "bind_exchange_accid";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["exchange_accid"] = accid;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+accid+args["time"]+App.key);
		Server.Instance.Get<ServerVO.BindExchangeAccidVO>(args,(receive)=>{
			App.Instance.exchange_accid = receive.exchange_accid;
			success();
		},()=>{
			fail();
		});
	}

	public void TransferCoinToExchange(float amount,Action success,Action fail){
		var args = new Dictionary<string,object>();
		args["a"] = "transfer_coin_to_exchange";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["amount"] = amount;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+amount+args["time"]+App.key);
		Server.Instance.Get<ServerVO.TransferCoinToExchangeVO>(args,(receive)=>{
			App.Instance.balance = receive.balance;
			App.Instance.exchange_balance = receive.exchange_balance;
			success();
		},()=>{
			fail();
		});
	}

	public void BindWallet(string addr,Action success,Action fail){
		var args = new Dictionary<string,object>();
		args["a"] = "bind_wallet";
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["wallet_addr"] = addr;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+""+addr+args["time"]+App.key);
		Server.Instance.Get<ServerVO.BindWalletVO>(args,(receive)=>{
			App.Instance.wallet_addr = receive.wallet_addr;
			success();
		},()=>{
			fail();
		});
	}

	public void TransferCoinToWallet(float amount,Action success,Action fail){
		var args = new Dictionary<string,object>();
		args["uid"] = App.Instance.uid;
		args["game_id"] = App.Instance.game.game_id;
		args["token"] = App.Instance.token;
		args["amount"] = amount;
		args["time"] = App.Instance.time;
		args["sign"] = Encrypt.Md5(App.Instance.uid+""+App.Instance.game.game_id+""+App.Instance.token+amount+""+args["time"]+App.key);
		Server.Instance.Get<ServerVO.TransferCoinToWalletVO>(args,(receive)=>{
			App.Instance.balance = receive.balance;
			App.Instance.exchange_balance = receive.exchange_balance;
			success();
		},()=>{
			fail();
		});
	}
}
