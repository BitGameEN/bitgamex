using System.Collections;
using System.Collections.Generic;
using UnityEngine;


/// <summary>
/// this script controls the movement of the enemy zombies in the game
/// </summary>
/// 

public class EnemyController : MonoBehaviour {

    public float enemySpeed; //maximum zombie speed

    Animator enemyAnimator; //reference to the animator attached

    //facing
    public GameObject enemyGraphic;

    bool canFlip= true;

    bool facingRight = false;
    float flipTime = 5f;
    float nextFlipChance = 0f;

    private float lerpspeed = 3f;
    //attacking
    public float chargeTime; // the time after which the enemy charges and runs towards player
    float startChargeTime;
    bool charging; //to check if enemy zombie is charging
    Rigidbody2D enemyRB; //reference to the rigidbody attached to enemy zombie


    // Use this for initialization
    void Start () {
        enemyAnimator = GetComponentInChildren<Animator>();
        enemyRB = GetComponent<Rigidbody2D>();
	}
	
	// Update is called once per frame
	void Update () {
		if(Time.time > nextFlipChance)
        {
            if (Random.Range(0, 10) >= 5)
                flipFacing();

            nextFlipChance = Time.time + flipTime;
               
        }
	}

    void OnTriggerEnter2D(Collider2D other)
    {
        if (other.tag == "Player")
        {
            if (facingRight && other.transform.position.x < transform.position.x)
            {
                flipFacing();
            }

            else if (!facingRight && other.transform.position.x > transform.position.x)
            {
                flipFacing();
                enemyRB.AddForce(new Vector2(1, 0) * enemySpeed);
            }
        }
        canFlip = false;
        charging = true;
        startChargeTime = Time.time + chargeTime;
    }

     void OnTriggerStay2D(Collider2D other)
    {
        if (other.tag == "Player")
        {
            if(startChargeTime < Time.time)
            {
                if (!facingRight)
                {
                    flipFacing();
                    enemyRB.AddForce(new Vector2(-1, 0) * enemySpeed);
                }
                else
                {
                    flipFacing();
                    enemyRB.AddForce(new Vector2(1, 0) * enemySpeed);
                }
                    enemyAnimator.SetBool("isCharging", charging);
            }
        }
    }

    void OnTriggerExit2D(Collider2D other)
        //when the player leaves the enemy detection collider
    {
        if (other.tag == "Player")
        {
            canFlip = true;
            charging = false;
            enemyRB.velocity = new Vector2(0f, 0f);
            enemyAnimator.SetBool("isCharging", charging);
        }
    }

    void flipFacing()
        //for flipping the facing of the enemy zombie
    {
        if (!canFlip) return;
        float facingX = enemyGraphic.transform.localScale.x;
        facingX *= -1f;
        enemyGraphic.transform.localScale = new Vector3(facingX, enemyGraphic.transform.localScale.y, enemyGraphic.transform.localScale.z);
        facingRight = !facingRight;
    }
}