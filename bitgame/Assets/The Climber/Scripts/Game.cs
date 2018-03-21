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
	}
}

public class GameData{
	public int id;
	public int score;
	public int time;
	public GameData(){
		this.id = 0;
		this.score = 0;
	}
}
