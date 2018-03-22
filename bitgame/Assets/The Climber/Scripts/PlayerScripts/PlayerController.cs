using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using System;
public class PlayerController : MonoBehaviour {

    private static PlayerController instance;

    public static PlayerController Instance
    {
        get
        {
            if (instance == null)
            {
                instance = GameObject.FindObjectOfType<PlayerController>();
            }
            return instance;
        }   
    }

    private Animator myAnim;

    [SerializeField]
    private float movementSpeed;

    private bool facingRight;

    [SerializeField]
    private Transform[] groundPoints;

    [SerializeField]
    private float groundRadius;

    [SerializeField]
    private LayerMask whatIsGround;

    [SerializeField]
    private float jumpForce;

    public  Rigidbody2D MyRigidBody { get; set; }


    public bool Jump { get; set; }

    public bool OnGround { get; set; }

    private float direction;
    private bool move;
    private float btnHorizontal;

    float fireRate = 0.3f;  // rate of firing
    float nextFire = 0f;  // the next fire time after one fire
    private AudioSource jumpSound;
    public GameObject playerDeathFX;
    public GameObject shareBtn, replayBtn, quitBtn, leftBtn, rightBtn, jumpBtn, pauseBtn, points, bestPoints;
    public Text saveDataLabel;
    // Use this for initialization
    void Start ()
    {

        if (instance == null)
        {
            instance = this;
        }

        facingRight = true;
        MyRigidBody = GetComponent<Rigidbody2D>();
        myAnim = GetComponent<Animator>();
        jumpSound = GameObject.Find("JumpSound").GetComponent<AudioSource>();
    }

    void Update()
    {
        HandleInput();
    }

    // Update is called once per frame
    void FixedUpdate ()
    {
        float horizontal = Input.GetAxis("Horizontal");

        OnGround = IsGrounded();
        
        if (move)
        {
            this.btnHorizontal = Mathf.Lerp(btnHorizontal, direction, Time.deltaTime * 2);
            HandleMovement(btnHorizontal);
            Flip(direction);
        }
        else
        {
            HandleMovement(horizontal);

            Flip(horizontal);
        }

        HandleLayers();

    }

    private void HandleMovement(float horizontal)
    {
        if (MyRigidBody.velocity.y < 0)
        {
            myAnim.SetBool("land", true);
        }

            MyRigidBody.velocity = new Vector2(horizontal * movementSpeed, MyRigidBody.velocity.y);
        

        if (Jump && MyRigidBody.velocity.y == 0)
        {
            MyRigidBody.AddForce(new Vector2(0, jumpForce));
            jumpSound.Play();
        }

        myAnim.SetFloat("speed", Mathf.Abs(horizontal));
    }


    private void HandleInput()
    {
        if (Input.GetKey(KeyCode.Space))
        {
            myAnim.SetTrigger("jump");
        }

    }

    private void Flip (float horizontal)
    {
        if (horizontal > 0 && !facingRight || horizontal < 0 && facingRight)
        {
            facingRight = !facingRight;

            Vector3 theScale = transform.localScale;

            theScale.x *= -1;

            transform.localScale = theScale;
        }
    }
    

    private bool IsGrounded ()
    {
        if (MyRigidBody.velocity.y <= 0)
        {
            foreach (Transform point in groundPoints)
            {
                Collider2D[] colliders = Physics2D.OverlapCircleAll(point.position, groundRadius, whatIsGround);

                for (int i = 0; i < colliders.Length; i++)
                {
                    if (colliders[i].gameObject != gameObject)
                    {
                        return true;
                    }
                }
            }
        }
        return false; 
    }

    private void HandleLayers()
    {
        if (!OnGround)
        {
            myAnim.SetLayerWeight(1, 1);
        }
        else
        {
            myAnim.SetLayerWeight(1, 0);
        }
    }


    void OnCollisionEnter2D(Collision2D other)
    {
        if (other.gameObject.tag == "MovingPlatform")
        {
            transform.parent = other.transform;
        }
    }

    public void MakeDead ()
    {
        Instantiate(playerDeathFX, transform.position, transform.rotation);
        gameObject.SetActive(false);
        //UnityAdsManager.instance.ShowAd();
        pauseBtn.SetActive(false);
        leftBtn.SetActive(false);
        rightBtn.SetActive(false);
        jumpBtn.SetActive(false);

        points.SetActive(true);
        bestPoints.SetActive(true);
        shareBtn.SetActive(true);
        replayBtn.SetActive(true);
        quitBtn.SetActive(true);
        ShareScreenShot.instance.TakeScreenshot();
        saveDataLabel.text = "游戏数据存盘中。。。";
		try{
			App.Instance.game.SaveGameData(Convert.ToInt32(ScoreManager.instance.pointsCount));
        }catch(Exception){
            App.Instance.game.SaveGameData(0);
        }
        API.Instance.SaveGame(()=>{
            saveDataLabel.text = "游戏数据存盘成功!";
        },(e)=>{
			saveDataLabel.text = "游戏数据存盘失败!";
			if(e != null){
				saveDataLabel.text += e.errno;
			}
        });
    }

    void OnCollisionExit2D(Collision2D other)
    {
        if (other.gameObject.tag == "MovingPlatform")
        {
            transform.parent = null;
        }
    }

    public void BtnJump ()
    {
        myAnim.SetTrigger("jump");
        Jump = true;
    }


    public void BtnMove (float direction)
    {
        this.direction = direction;
        this.move = true;
    }

    public void BtnStopMove ()
    {
        this.direction = 0;
        this.btnHorizontal = 0; 
        move = false;
    }
}
