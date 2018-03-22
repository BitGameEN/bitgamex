using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Game{
	public const int id = 1;
	public int game_id{
		get{
			return id;
		}
	}
	private GameData _data = new GameData();
	public string data{
		get{
			return JsonUtility.ToJson(this._data);
		}
		set{
			this._data = JsonUtility.FromJson<GameData>(value);
		}
	}

	public void SaveGameData(int score){
		if(this._data == null){
			this._data = new GameData();
		}
		this._data.score = score;
		this._data.time = App.Instance.time;
	}
}

public class GameData{
	public int score;
	public long time;
	public GameData(){
		this.score = 0;
		this.time = 0;
	}
}
